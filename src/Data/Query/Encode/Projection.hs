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
import qualified Data.Functor.Contravariant.Coyoneda as Contravariant.Coyoneda
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Encode.Types as Types
import qualified Data.Query.Schema.Types as Types
import qualified Data.Query.Shape as Shape
import qualified Data.SOP as SOP
import           Data.Text (Text)

data ProjectionError
  = EnumItemsMismatch (HashSet.HashSet Text) (HashSet.HashSet Text)
  | MissingVariantConstructor Text
  | TargetFieldIsMandatory
  | ExtraMandatoryFields (HashSet.HashSet Text)
  | forall a. CompleteMismatch Shape.Shape (Encode.Encoder a)

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

projectEncoder
  :: Shape.Shape
  -> Encode.Encoder a
  -> Either LocatedProjectionError (Encode.Encoder a)
projectEncoder target sourceEncoder =
  case sourceEncoder of
    Types.Encoder (Contravariant.Coyoneda.Coyoneda f source) ->
      Types.Encoder . Contravariant.Coyoneda.Coyoneda f <$>
        projectEncoderBase target source

projectEncoderBase
  :: Shape.Shape
  -> Types.EncoderBase a
  -> Either LocatedProjectionError (Types.EncoderBase a)
projectEncoderBase fixTarget@(Fix target) source =
  case (target, source) of
    (Shape.Bool, Types.BoolEncoder) ->
      pure source

    (Shape.Number, Types.NumberEncoder) ->
      pure source

    (Shape.String, Types.StringEncoder) ->
      pure source

    (Shape.Array targetItems, Types.ArrayEncoder sourceItems) ->
      nestProjectionError Types.ArrayPath $
        Types.ArrayEncoder <$> projectEncoder targetItems sourceItems

    (Shape.StringMap targetItems, Types.StringMapEncoder sourceItems) ->
      nestProjectionError Types.StringMapPath $
        Types.StringMapEncoder <$> projectEncoder targetItems sourceItems

    (Shape.Enum targetsList, Types.EnumEncoder sourceItems) -> do
      let
        targets = HashSet.fromList targetsList

        sources =
          HashSet.fromList
          $ SOP.hcollapse
          $ SOP.hmap (\(Types.ItemEncoder name) -> SOP.K name) sourceItems

      unless (HashSet.isSubsetOf sources targets) $
        throwProjectionError $ EnumItemsMismatch targets sources

      pure source

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

      pure $ Types.VariantEncoder sourceConstructors

    (Shape.Record targetFields, Types.RecordEncoder sourceFields) -> do
      let
        mandatoryExtraFields =
          HashMap.filter (not . Shape.fieldShape_optional) $
            HashMap.difference targetFields sourceFields

      unless (null mandatoryExtraFields) $
        throwProjectionError $ ExtraMandatoryFields $ HashMap.keysSet mandatoryExtraFields

      fmap Types.RecordEncoder $ sequenceA $
        HashMap.intersectionWithKey
          (\name target source ->
            nestProjectionError (Types.FieldPath name) $ projectFieldEncoder target source
          )
          targetFields
          sourceFields

    _ ->
      throwProjectionError $ CompleteMismatch fixTarget $ Types.Encoder $
        Contravariant.Coyoneda.liftCoyoneda source

projectFieldEncoder
  :: Shape.FieldShapeF Shape.Shape
  -> Types.FieldEncoder a
  -> Either LocatedProjectionError (Types.FieldEncoder a)
projectFieldEncoder (Shape.FieldShape target optional) (Types.FieldEncoder sourceField) =
  case sourceField of
    Contravariant.Coyoneda.Coyoneda f sourceSelector -> do
      let rewrap = Types.FieldEncoder . Contravariant.Coyoneda.Coyoneda f
      case sourceSelector of
        Types.OptionalFieldSelector source
          | optional  -> rewrap . Types.OptionalFieldSelector <$> projectEncoder target source
          | otherwise -> throwProjectionError TargetFieldIsMandatory

        Types.MandatoryFieldSelector source ->
          rewrap . Types.MandatoryFieldSelector <$> projectEncoder target source
