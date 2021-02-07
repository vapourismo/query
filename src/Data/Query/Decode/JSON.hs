{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query.Decode.JSON
  ( Types.DecodeContext (..)
  , evalQuery
  , evalDecoder
  , evalFieldDecoder
  , parseJsonWith
  , parseJson
  )
where

import           Control.Applicative.Free (runAp)
import qualified Data.Aeson.Types as Aeson
import           Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, lowerCoyoneda)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Decode.Types as Types
import qualified Data.Query.Primitives.JSON as Primitives
import qualified Data.Query.Types as Types
import qualified Data.Query.Value as Value

evalFieldDecoder
  :: forall a m
  .  Types.DecodeContext m
  => Decode.FieldsDecoder a
  -> Value.Object
  -> m a
evalFieldDecoder fieldDecoder queryObject =
  runAp reduce $ Types.unFieldDecoder fieldDecoder
  where
    reduce :: forall x. Types.FieldQuery x -> m x
    reduce = \case
      Types.MandatoryFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue ->
            Types.nestDecodeError (Types.FieldPath name) $ evalQuery query fieldValue

          Nothing ->
            Types.throwDecodeError $ Types.MissingField name

      Types.OptionalFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue ->
            Types.nestDecodeError (Types.FieldPath name) $ Just <$> evalQuery query fieldValue

          Nothing ->
            pure Nothing

evalDecoder
  :: Types.DecodeContext m
  => Decode.Decoder b
  -> Value.NoCallValue
  -> m b
evalDecoder decoder value =
  lowerCoyoneda $ hoistCoyoneda (`evalDecoderBase` value) $ Types.unDecoder decoder

evalDecoderBase
  :: Types.DecodeContext m
  => Types.DecoderBase a
  -> Value.NoCallValue
  -> m a
evalDecoderBase decoder queryValue =
  case decoder of
    Types.PrimitiveDecoder prim ->
      Primitives.decodePrimitive prim queryValue

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
              Types.throwDecodeError $ Types.UnknownEnum (HashMap.keysSet valueMapping) string

        queryValue -> throwUnexpected queryValue

    Types.VariantDecoder allVariants ->
      case queryValue of
        Value.Object fields ->
          case HashMap.toList (HashMap.intersectionWith (,) allVariants fields) of
            [] ->
              Types.throwDecodeError $
                Types.NoMatchingConstructor (HashMap.keysSet allVariants) fields

            [(name, (Types.ConstructorQuery (Coyoneda f decoder), queryValue))] ->
              Types.nestDecodeError (Types.ConstructorPath name) $
                f <$> evalQuery decoder queryValue

            matches ->
              Types.throwDecodeError $ Types.MultipleConstructors $
                map
                  (\(name, (decoder, value)) -> Types.ConstructorMatch name decoder value)
                  matches

        queryValue -> throwUnexpected queryValue

    Types.RecordDecoder fieldQueries ->
      case queryValue of
        Value.Object fields -> evalFieldDecoder fieldQueries fields
        queryValue          -> throwUnexpected queryValue

  where
    throwUnexpected = Types.throwDecodeError . Types.UnexpectedInput decoder

evalQuery
  :: Types.DecodeContext m
  => Decode.Query a
  -> Value.Value
  -> m a
evalQuery (Types.Query typeRep mbDecoder) = \case
  Value.Call (Value.CallValue name args) ->
    Types.callFunction typeRep name args

  Value.NoCall queryValue ->
    case mbDecoder of
      Just decoder -> evalDecoder decoder queryValue
      Nothing -> Types.throwDecodeError $ Types.UndecodableNeedsCall typeRep queryValue

parseJsonWith :: Decode.Decoder a -> Aeson.Value -> Aeson.Parser a
parseJsonWith decoder value =
  evalDecoder decoder $ Value.toNoCallValue value

parseJson :: Decode.HasDecoder a => Aeson.Value -> Aeson.Parser a
parseJson = parseJsonWith Decode.decoder
