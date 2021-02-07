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
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Value as Value
import qualified Data.Text as Text

decodeNumber
  :: Primitives.NumberInfo a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodeNumber info value =
  Primitives.reifyNumberConstraints (Primitives.numberInfo_format info) $ do
    number <-
      case value of
        Value.Number number ->
          case Aeson.fromJSON (Aeson.Number number) of
            Aeson.Success x -> Right x
            Aeson.Error msg -> Left $ Decode.BadPrimitive value $ Text.pack msg

        _ -> Left $ Decode.UnexpectedInput decoder value

    let badPrimError = Left . Decode.BadPrimitive value . Text.pack

    case Primitives.numberInfo_lowerLimit info of
      Primitives.InclusiveLimit limit | number < limit ->
        badPrimError $ show number <> " < " <> show limit

      Primitives.ExclusiveLimit limit | number <= limit ->
        badPrimError $ show number <> " <= " <> show limit

      _ -> Right ()

    case Primitives.numberInfo_upperLimit info of
      Primitives.InclusiveLimit limit | number > limit ->
        badPrimError $ show number <> " > " <> show limit

      Primitives.ExclusiveLimit limit | number >= limit ->
        badPrimError $ show number <> " >= " <> show limit

      _ -> Right ()

    Right number
  where
    decoder = Decode.PrimitiveDecoder $ Primitives.Number info

encodeNumber
  :: Primitives.NumberInfo a
  -> a
  -> Aeson.Value
encodeNumber info =
  Primitives.reifyNumberConstraints (Primitives.numberInfo_format info) Aeson.toJSON

decodeInteger
  :: Primitives.IntegerInfo a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodeInteger info value =
  Primitives.reifyIntegerConstraints (Primitives.integerInfo_format info) $ do
    integer <-
      case value of
        Value.Number integer ->
          case Aeson.fromJSON (Aeson.Number integer) of
            Aeson.Success x -> pure x
            Aeson.Error msg -> Left $ Decode.BadPrimitive value $ Text.pack msg

        _ -> Left $ Decode.UnexpectedInput decoder value

    let badPrimError = Left . Decode.BadPrimitive value . Text.pack

    case Primitives.integerInfo_lowerLimit info of
      Primitives.InclusiveLimit limit | integer < limit ->
        badPrimError $ show integer <> " < " <> show limit

      Primitives.ExclusiveLimit limit | integer <= limit ->
        badPrimError $ show integer <> " <= " <> show limit

      _ -> Right ()

    case Primitives.integerInfo_upperLimit info of
      Primitives.InclusiveLimit limit | integer > limit ->
        badPrimError $ show integer <> " > " <> show limit

      Primitives.ExclusiveLimit limit | integer >= limit ->
        badPrimError $ show integer <> " >= " <> show limit

      _ -> Right ()

    case Primitives.integerInfo_multipleOf info of
      Just multiple | mod integer multiple /= 0 ->
        badPrimError $ show integer <> " is not a multiple of " <> show multiple

      _ -> Right ()

    Right integer
  where
    decoder = Decode.PrimitiveDecoder $ Primitives.Integer info

encodeInteger
  :: Primitives.IntegerInfo a
  -> a
  -> Aeson.Value
encodeInteger info =
  Primitives.reifyIntegerConstraints (Primitives.integerInfo_format info) Aeson.toJSON

decodeString
  :: forall a
  .  Primitives.StringFormat a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodeString format value =
  case value of
    Value.String string ->
      case parse (Aeson.String string) of
        Aeson.Success x -> pure x
        Aeson.Error msg -> Left $ Decode.BadPrimitive value $ Text.pack msg

    _ -> Left $ Decode.UnexpectedInput decoder value
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
  :: Primitives.Primitive a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodePrimitive = \case
  Primitives.Boolean -> \case
    Value.Bool bool -> pure bool
    value -> Left $ Decode.UnexpectedInput (Decode.PrimitiveDecoder Primitives.Boolean) value

  Primitives.Number info -> decodeNumber info
  Primitives.Integer info -> decodeInteger info
  Primitives.String format -> decodeString format

encodePrimitive :: Primitives.Primitive a -> a -> Aeson.Value
encodePrimitive = \case
  Primitives.Boolean -> Aeson.toJSON
  Primitives.Number info -> encodeNumber info
  Primitives.Integer info -> encodeInteger info
  Primitives.String format -> encodeString format
