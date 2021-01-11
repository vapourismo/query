{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query.Decode.JSON
  ( evalQuery
  , evalDecoder
  , evalFieldDecoder
  , parseJson
  , parseJsonWith
  )
where

import           Control.Applicative.Free (runAp)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, lowerCoyoneda)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Decode.Types as Types
import qualified Data.Query.Evaluate as Evaluate
import qualified Data.Query.Schema.Types as Schema
import qualified Data.Query.Value as Value

throwError :: Types.DecodeError -> Evaluate.Evaluate m a
throwError = Evaluate.throwQueryError . Evaluate.DecodeError

evalFieldDecoder
  :: forall a m
  .  Applicative m
  => Decode.FieldsDecoder a
  -> Value.Object
  -> Evaluate.Evaluate m a
evalFieldDecoder fieldDecoder queryObject =
  runAp reduce $ Types.unFieldDecoder fieldDecoder
  where
    reduce :: forall x. Types.FieldQuery x -> Evaluate.Evaluate m x
    reduce = \case
      Types.MandatoryFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue ->
            Evaluate.nestQueryError (Schema.FieldPath name) $ evalQuery query fieldValue

          Nothing ->
            throwError $ Types.MissingField name

      Types.OptionalFieldQuery name query ->
        case HashMap.lookup name queryObject of
          Just fieldValue ->
            Evaluate.nestQueryError (Schema.FieldPath name) $ Just <$> evalQuery query fieldValue

          Nothing ->
            pure Nothing

evalDecoder
  :: Applicative m
  => Decode.Decoder b
  -> Value.NoCallValue
  -> Evaluate.Evaluate m b
evalDecoder decoder value =
  lowerCoyoneda $ hoistCoyoneda (`evalDecoderBase` value) $ Types.unDecoder decoder

evalDecoderBase
  :: Applicative m
  => Types.DecoderBase a
  -> Value.NoCallValue
  -> Evaluate.Evaluate m a
evalDecoderBase decoder queryValue =
  case decoder of
    Types.BoolDecoder ->
      case queryValue of
        Value.Bool value -> pure value
        queryValue       -> throwUnexpected queryValue

    Types.NumberDecoder ->
      case queryValue of
        Value.Number value -> pure value
        queryValue         -> throwUnexpected queryValue

    Types.StringDecoder ->
      case queryValue of
        Value.String value -> pure value
        queryValue         -> throwUnexpected queryValue

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
            Just value -> pure value
            Nothing    -> throwError $ Types.UnknownEnum (HashMap.keysSet valueMapping) string

        queryValue -> throwUnexpected queryValue

    Types.VariantDecoder allVariants ->
      case queryValue of
        Value.Object fields ->
          case HashMap.toList (HashMap.intersectionWith (,) allVariants fields) of
            [] -> throwError $ Types.NoMatchingConstructor (HashMap.keysSet allVariants) fields


            [(name, (Types.ConstructorQuery (Coyoneda f decoder), queryValue))] ->
              Evaluate.nestQueryError (Schema.ConstructorPath name) $
                f <$> evalQuery decoder queryValue

            matches ->
              throwError
                $ Types.MultipleConstructors
                $ map (\(name, (decoder, value)) -> Types.ConstructorMatch name decoder value) matches

        queryValue -> throwUnexpected queryValue

    Types.RecordDecoder fieldQueries ->
      case queryValue of
        Value.Object fields -> evalFieldDecoder fieldQueries fields
        queryValue          -> throwUnexpected queryValue

  where
    throwUnexpected = throwError . Types.UnexpectedInput decoder

evalQuery
  :: Applicative m
  => Decode.Query a
  -> Value.Value
  -> Evaluate.Evaluate m a
evalQuery (Types.Query typeRep mbDecoder) = \case
  Value.Call (Value.CallValue name args) ->
    Evaluate.callFunction typeRep name args

  Value.NoCall queryValue ->
    case mbDecoder of
      Just decoder -> evalDecoder decoder queryValue
      Nothing -> throwError $ Types.UndecodableNeedsCall typeRep queryValue

aesonResolver :: Evaluate.Resolver Aeson.Parser
aesonResolver =
  Evaluate.Resolver
    (error "Calls are not supported when parsing JSON")
    (error "To-level calls are not supported when parsing JSON")

parseJsonWith :: Decode.Decoder a -> Aeson.Value -> Aeson.Parser a
parseJsonWith decoder value =
  either (fail . show) id $ Evaluate.runEvaluate aesonResolver $
    evalDecoder decoder $ Value.toNoCallValue value

parseJson :: Decode.HasDecoder a => Aeson.Value -> Aeson.Parser a
parseJson = parseJsonWith Decode.decoder
