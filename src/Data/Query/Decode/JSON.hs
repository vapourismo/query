{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query.Decode.JSON
  ( Decoded (..)
  , evalQuery
  , evalDecoder
  , evalFieldDecoder
  , parseJsonWith
  , parseJson
  )
where

import           Control.Applicative.Free (runAp)
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (first))
import           Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, lowerCoyoneda)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Decode.Types as Types
import qualified Data.Query.Primitives.JSON as Primitives
import qualified Data.Query.Types as Types
import qualified Data.Query.Value as Value

newtype Decoded a = Decoded
  { runDecoded
      :: forall f
      .  Types.CanCallFunction f
      => Either (Types.Located Types.DecodeError) (f a)
  }

instance Functor Decoded where
  fmap f (Decoded inner) = Decoded $ fmap f <$> inner

instance Applicative Decoded where
  pure x = Decoded $ Right $ pure x

  Decoded f <*> Decoded x = Decoded $ (<*>) <$> f <*> x

throwError :: Types.DecodeError -> Decoded a
throwError error = Decoded $ Left $ Types.Located [] error

nestError :: Types.Path -> Decoded a -> Decoded a
nestError path (Decoded inner) =
  Decoded $ first addPath inner
  where
    addPath (Types.Located paths x) = Types.Located (path : paths) x

evalFieldDecoder
  :: forall a
  .  Decode.FieldsDecoder a
  -> Value.Object
  -> Decoded a
evalFieldDecoder fieldDecoder queryObject =
  runAp reduce $ Types.unFieldDecoder fieldDecoder
  where
    reduce :: forall x. Types.FieldQuery x -> Decoded x
    reduce = \case
      Types.MandatoryFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue -> nestError (Types.FieldPath name) $ evalQuery query fieldValue
          Nothing -> throwError $ Types.MissingField name

      Types.OptionalFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue -> nestError (Types.FieldPath name) $ Just <$> evalQuery query fieldValue
          Nothing -> pure Nothing

evalDecoder
  :: Decode.Decoder b
  -> Value.NoCallValue
  -> Decoded b
evalDecoder decoder value =
  lowerCoyoneda $ hoistCoyoneda (`evalDecoderBase` value) $ Types.unDecoder decoder

evalDecoderBase
  :: Types.DecoderBase a
  -> Value.NoCallValue
  -> Decoded a
evalDecoderBase decoder queryValue =
  case decoder of
    Types.PrimitiveDecoder prim ->
      either throwError pure $ Primitives.decodePrimitive prim queryValue

    Types.NullableDecoder decoder ->
      case queryValue of
        Value.Null -> pure Nothing
        queryValue -> Just <$> evalDecoderBase decoder queryValue

    Types.ArrayDecoder decoder ->
      case queryValue of
        Value.Array items -> traverse (evalQuery decoder) items
        queryValue        -> throwUnexpected queryValue

    Types.StringMapDecoder decoder ->
      case queryValue of
        Value.Object items -> traverse (evalQuery decoder) items
        queryValue         -> throwUnexpected queryValue

    Types.EnumDecoder valueMapping ->
      case queryValue of
        Value.String string ->
          case HashMap.lookup string valueMapping of
            Just value ->
              pure value

            Nothing ->
              throwError $ Types.UnknownEnum (HashMap.keysSet valueMapping) string

        queryValue -> throwUnexpected queryValue

    Types.VariantDecoder allVariants ->
      case queryValue of
        Value.Object fields ->
          case HashMap.toList (HashMap.intersectionWith (,) allVariants fields) of
            [] ->
              throwError $ Types.NoMatchingConstructor (HashMap.keysSet allVariants) fields

            [(name, (Types.ConstructorQuery (Coyoneda f decoder), queryValue))] ->
              nestError (Types.ConstructorPath name) $ f <$> evalQuery decoder queryValue

            matches ->
              throwError $ Types.MultipleConstructors $
                map
                  (\(name, (decoder, value)) -> Types.ConstructorMatch name decoder value)
                  matches

        queryValue -> throwUnexpected queryValue

    Types.RecordDecoder fieldQueries ->
      case queryValue of
        Value.Object fields -> evalFieldDecoder fieldQueries fields
        queryValue          -> throwUnexpected queryValue

  where
    throwUnexpected = throwError . Types.UnexpectedInput decoder

evalQuery
  :: Decode.Query a
  -> Value.Value
  -> Decoded a
evalQuery (Types.Query typeRep mbDecoder) = \case
  Value.Call (Value.CallValue name args) ->
    Decoded $ Right $ Types.callFunction name typeRep args

  Value.NoCall queryValue ->
    case mbDecoder of
      Just decoder -> evalDecoder decoder queryValue
      Nothing -> throwError $ Types.UndecodableNeedsCall typeRep queryValue

parseJsonWith :: Decode.Decoder a -> Aeson.Value -> Aeson.Parser a
parseJsonWith decoder value =
  either (fail . show) id
  $ runDecoded
  $ evalDecoder decoder
  $ Value.toNoCallValue value

parseJson :: Decode.HasDecoder a => Aeson.Value -> Aeson.Parser a
parseJson = parseJsonWith Decode.decoder
