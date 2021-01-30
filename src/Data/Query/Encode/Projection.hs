{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Query.Encode.Projection
  ( ProjectionError (..)
  , LocatedProjectionError (..)
  , projectEncoder
  )
where

import           Control.Monad (unless)
import           Data.Bifunctor (Bifunctor (first))
import           Data.Fix (Fix (Fix))
import           Data.Functor.Contravariant (Contravariant (contramap))
import qualified Data.Functor.Contravariant.Coyoneda as Contravariant
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Encode.Types as Types
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Schema.Types as Types
import qualified Data.Query.Shape as Shape
import qualified Data.SOP as SOP
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding as Text

data ProjectionError
  = EnumItemsMismatch (HashSet.HashSet Text) (HashSet.HashSet Text)
  | MissingVariantConstructor Text
  | TargetFieldIsMandatory
  | ExtraMandatoryFields (HashSet.HashSet Text)
  | forall a. CompleteMismatch Shape.Shape (Encode.Encoder a)
  | IncompatiblePrimitives Primitives.SomePrimitive Primitives.SomePrimitive

deriving instance Show ProjectionError

data LocatedProjectionError = LocatedProjectionError [Types.Path] ProjectionError
  deriving Show

throwProjectionError
  :: ProjectionError
  -> Either LocatedProjectionError a
throwProjectionError =
  Left . LocatedProjectionError []

nestProjectionError
  :: Types.Path
  -> Either LocatedProjectionError a
  -> Either LocatedProjectionError a
nestProjectionError path =
  first $ \(LocatedProjectionError paths error) ->
    LocatedProjectionError (path : paths) error

-- | Project a primitive type to match the target.
projectPrimitive
  :: Primitives.Primitive a -- ^ Target
  -> Primitives.Primitive b -- ^ Subject
  -> Either LocatedProjectionError (Contravariant.Coyoneda Primitives.Primitive b)
projectPrimitive target source =
  case (target, source) of
    _ | Primitives.SomePrimitive target == Primitives.SomePrimitive source ->
      -- If both sides are equal then everything is fine.
      Right $ Contravariant.liftCoyoneda source

    -- TODO: There are way more projection possibilities waiting to be implemented.

    (Primitives.String targetFormat, Primitives.String sourceFormat)
      | Primitives.NoStringFormat <- targetFormat ->
        -- The target has no expectation of the string.
        Right $ Contravariant.liftCoyoneda source

      | Primitives.ByteFormat <- targetFormat ->
        -- We can base64-encode the string and with that match the target expectation.
        let
          convert =
            case sourceFormat of
              Primitives.NoStringFormat -> Text.encodeUtf8
              Primitives.ByteFormat -> id
              Primitives.BinaryFormat -> Text.encodeUtf8
              Primitives.DateFormat -> Text.encodeUtf8 . pack . show
              Primitives.DateTimeFormat -> Text.encodeUtf8 . pack . show
              Primitives.PasswordFormat -> Text.encodeUtf8
        in
          Right $ Contravariant.Coyoneda convert $ Primitives.String Primitives.ByteFormat

      | Primitives.BinaryFormat <- targetFormat ->
        -- In this context, binary does not mean anything special.
        Right $ Contravariant.liftCoyoneda source

      | Primitives.PasswordFormat <- targetFormat ->
        -- The password format is a UI hint.
        Right $ Contravariant.liftCoyoneda source

    _ -> throwProjectionError $
      IncompatiblePrimitives
        (Primitives.SomePrimitive target)
        (Primitives.SomePrimitive source)

-- | Project an encoder to match the given shape.
projectEncoder
  :: Shape.Shape
  -> Encode.Encoder a
  -> Either LocatedProjectionError (Encode.Encoder a)
projectEncoder target sourceEncoder =
  case sourceEncoder of
    Types.Encoder (Contravariant.Coyoneda f source) ->
      contramap f <$> projectEncoderBase target source

-- | Project an encoder base to match the given shape.
projectEncoderBase
  :: Shape.Shape
  -> Types.EncoderBase a
  -> Either LocatedProjectionError (Types.Encoder a)
projectEncoderBase fixTarget@(Fix target) source =
  case (target, source) of
    (Shape.Primitive (Primitives.SomePrimitive lhs), Types.PrimitiveEncoder rhs) -> do
      Contravariant.Coyoneda f prim <- projectPrimitive lhs rhs
      pure $ contramap f $ liftSimple $ Types.PrimitiveEncoder prim

    (Shape.Array targetItems, Types.ArrayEncoder sourceItems) ->
      nestProjectionError Types.ArrayPath $
        liftSimple . Types.ArrayEncoder <$> projectEncoder targetItems sourceItems

    (Shape.StringMap targetItems, Types.StringMapEncoder sourceItems) ->
      nestProjectionError Types.StringMapPath $
        liftSimple . Types.StringMapEncoder <$> projectEncoder targetItems sourceItems

    (Shape.Enum targetsList, Types.EnumEncoder sourceItems) -> do
      let
        targets = HashSet.fromList targetsList

        sources =
          HashSet.fromList
          $ SOP.hcollapse
          $ SOP.hmap (\(Types.ItemEncoder name) -> SOP.K name) sourceItems

      unless (HashSet.isSubsetOf sources targets) $
        throwProjectionError $ EnumItemsMismatch targets sources

      pure $ liftSimple source

    (Shape.Variant targets, Types.VariantEncoder sourceConstructors) -> do
      sourceConstructors <-
        SOP.htraverse'
          (\(Types.ConstructorEncoder name encoder) -> do
            target <-
              case HashMap.lookup name targets of
                Nothing     -> throwProjectionError $ MissingVariantConstructor name
                Just target -> pure target

            nestProjectionError (Types.FieldPath name) $
              Types.ConstructorEncoder name <$> projectEncoder target encoder
          )
          sourceConstructors

      pure $ liftSimple $ Types.VariantEncoder sourceConstructors

    (Shape.Record targetFields, Types.RecordEncoder sourceFields) -> do
      let
        mandatoryExtraFields =
          HashMap.filter (not . Shape.fieldShape_optional) $
            HashMap.difference targetFields sourceFields

      unless (null mandatoryExtraFields) $
        throwProjectionError $ ExtraMandatoryFields $ HashMap.keysSet mandatoryExtraFields

      fmap (liftSimple . Types.RecordEncoder) $ sequenceA $
        HashMap.intersectionWithKey
          (\name target source ->
            nestProjectionError (Types.FieldPath name) $ projectFieldEncoder target source
          )
          targetFields
          sourceFields

    _ ->
      throwProjectionError $ CompleteMismatch fixTarget $ Types.Encoder $
        Contravariant.liftCoyoneda source

    where
      liftSimple :: Types.EncoderBase x -> Types.Encoder x
      liftSimple = Types.Encoder . Contravariant.liftCoyoneda

-- | Project a field encoder to match the given field shape.
projectFieldEncoder
  :: Shape.FieldShapeF Shape.Shape
  -> Types.FieldEncoder a
  -> Either LocatedProjectionError (Types.FieldEncoder a)
projectFieldEncoder (Shape.FieldShape target optional) (Types.FieldEncoder sourceField) =
  case sourceField of
    Contravariant.Coyoneda f sourceSelector -> do
      let rewrap = Types.FieldEncoder . Contravariant.Coyoneda f
      case sourceSelector of
        Types.OptionalFieldSelector source
          | optional  -> rewrap . Types.OptionalFieldSelector <$> projectEncoder target source
          | otherwise -> throwProjectionError TargetFieldIsMandatory

        Types.MandatoryFieldSelector source ->
          rewrap . Types.MandatoryFieldSelector <$> projectEncoder target source
