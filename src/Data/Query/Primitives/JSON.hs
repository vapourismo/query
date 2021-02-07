{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Query.Primitives.JSON
  ( decodeNumber
  , encodeNumber
  , decodeInteger
  , encodeInteger
  , decodeString
  , encodeString
  , decodePrimitive
  , encodePrimitive
  )
where

import qualified Codec.Base64 as Base64
import           Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (Bifunctor (first))
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Evaluate as Evaluate
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Value as Value
import qualified Data.Text as Text

throwDecodeError :: Decode.DecodeError -> Evaluate.Evaluate m a
throwDecodeError = Evaluate.throwEvaluateError . Evaluate.DecodeError

decodeNumber
  :: Applicative m
  => Primitives.NumberInfo a
  -> Value.NoCallValue
  -> Evaluate.Evaluate m a
decodeNumber info value =
  Primitives.reifyNumberConstraints (Primitives.numberInfo_format info) $ lift $ do
    number <-
      case value of
        Value.Number number ->
          case Aeson.fromJSON (Aeson.Number number) of
            Aeson.Success x -> Right x
            Aeson.Error msg -> Left $ Decode.BadPrimitive value $ Text.pack msg

        _ -> Left $ Decode.UnexpectedInput decoder value

    first (Decode.BadPrimitive value . Text.pack) $ do
      case Primitives.numberInfo_lowerLimit info of
        Primitives.InclusiveLimit limit | number < limit ->
          Left $ show number <> " < " <> show limit

        Primitives.ExclusiveLimit limit | number <= limit ->
          Left $ show number <> " <= " <> show limit

        _ -> pure ()

      case Primitives.numberInfo_upperLimit info of
        Primitives.InclusiveLimit limit | number > limit ->
          Left $ show number <> " > " <> show limit

        Primitives.ExclusiveLimit limit | number >= limit ->
          Left $ show number <> " >= " <> show limit

        _ -> pure ()

    pure number
  where
    decoder = Decode.PrimitiveDecoder $ Primitives.Number info

    lift = either throwDecodeError pure

encodeNumber
  :: Primitives.NumberInfo a
  -> a
  -> Aeson.Value
encodeNumber info =
  Primitives.reifyNumberConstraints (Primitives.numberInfo_format info) Aeson.toJSON

decodeInteger
  :: Applicative m
  => Primitives.IntegerInfo a
  -> Value.NoCallValue
  -> Evaluate.Evaluate m a
decodeInteger info value =
  Primitives.reifyIntegerConstraints (Primitives.integerInfo_format info) $ lift $ do
    integer <-
      case value of
        Value.Number integer ->
          case Aeson.fromJSON (Aeson.Number integer) of
            Aeson.Success x -> Right x
            Aeson.Error msg -> Left $ Decode.BadPrimitive value $ Text.pack msg

        _ -> Left $ Decode.UnexpectedInput decoder value

    first (Decode.BadPrimitive value . Text.pack) $ do
      case Primitives.integerInfo_lowerLimit info of
        Primitives.InclusiveLimit limit | integer < limit ->
          Left $ show integer <> " < " <> show limit

        Primitives.ExclusiveLimit limit | integer <= limit ->
          Left $ show integer <> " <= " <> show limit

        _ -> pure ()

      case Primitives.integerInfo_upperLimit info of
        Primitives.InclusiveLimit limit | integer > limit ->
          Left $ show integer <> " > " <> show limit

        Primitives.ExclusiveLimit limit | integer >= limit ->
          Left $ show integer <> " >= " <> show limit

        _ -> Right ()

      case Primitives.integerInfo_multipleOf info of
        Just multiple | mod integer multiple /= 0 ->
          Left $ show integer <> " is not a multiple of " <> show multiple

        _ -> Right ()

    pure integer
  where
    decoder = Decode.PrimitiveDecoder $ Primitives.Integer info

    lift = either throwDecodeError pure

encodeInteger
  :: Primitives.IntegerInfo a
  -> a
  -> Aeson.Value
encodeInteger info =
  Primitives.reifyIntegerConstraints (Primitives.integerInfo_format info) Aeson.toJSON

decodeString
  :: forall a m
  .  Applicative m
  => Primitives.StringFormat a
  -> Value.NoCallValue
  -> Evaluate.Evaluate m a
decodeString format value =
  case value of
    Value.String string ->
      case parse (Aeson.String string) of
        Aeson.Success x -> pure x
        Aeson.Error msg -> throwDecodeError $ Decode.BadPrimitive value $ Text.pack msg

    _ -> throwDecodeError $ Decode.UnexpectedInput decoder value
  where
    decoder = Decode.PrimitiveDecoder $ Primitives.String format

    parse :: Aeson.Value -> Aeson.Result a
    parse =
      case format of
        Primitives.NoStringFormat -> Aeson.fromJSON
        Primitives.ByteFormat -> either fail pure . Base64.decode @Text.Text <=< Aeson.fromJSON
        Primitives.BinaryFormat -> Aeson.fromJSON
        Primitives.DateFormat -> Aeson.fromJSON
        Primitives.DateTimeFormat -> Aeson.fromJSON
        Primitives.PasswordFormat -> Aeson.fromJSON

encodeString :: Primitives.StringFormat a -> a -> Aeson.Value
encodeString = \case
  Primitives.NoStringFormat -> Aeson.toJSON
  Primitives.ByteFormat -> Aeson.toJSON @Text.Text . Base64.encode
  Primitives.BinaryFormat -> Aeson.toJSON
  Primitives.DateFormat -> Aeson.toJSON
  Primitives.DateTimeFormat -> Aeson.toJSON
  Primitives.PasswordFormat -> Aeson.toJSON

decodePrimitive
  :: Applicative m
  => Primitives.Primitive a
  -> Value.NoCallValue
  -> Evaluate.Evaluate m a
decodePrimitive = \case
  Primitives.Boolean -> \case
    Value.Bool bool -> pure bool
    value -> throwDecodeError $
      Decode.UnexpectedInput (Decode.PrimitiveDecoder Primitives.Boolean) value

  Primitives.Number info -> decodeNumber info
  Primitives.Integer info -> decodeInteger info
  Primitives.String format -> decodeString format

encodePrimitive :: Primitives.Primitive a -> a -> Aeson.Value
encodePrimitive = \case
  Primitives.Boolean -> Aeson.toJSON
  Primitives.Number info -> encodeNumber info
  Primitives.Integer info -> encodeInteger info
  Primitives.String format -> encodeString format
