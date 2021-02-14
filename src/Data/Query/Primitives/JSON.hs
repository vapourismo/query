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
  :: Primitives.NumberFormat a
  -> Primitives.NumberInfo a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodeNumber format info value =
  Primitives.reifyNumberConstraints format $ do
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
    decoder = Decode.PrimitiveDecoder $ Primitives.Number format info

encodeNumber
  :: Primitives.NumberFormat a
  -> a
  -> Aeson.Value
encodeNumber format =
  Primitives.reifyNumberConstraints format Aeson.toJSON

decodeInteger
  :: Primitives.IntegerFormat a
  -> Primitives.IntegerInfo a
  -> Value.NoCallValue
  -> Either Decode.DecodeError a
decodeInteger format info value =
  Primitives.reifyIntegerConstraints format $ do
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
    decoder = Decode.PrimitiveDecoder $ Primitives.Integer format info

encodeInteger
  :: Primitives.IntegerFormat a
  -> a
  -> Aeson.Value
encodeInteger format =
  Primitives.reifyIntegerConstraints format Aeson.toJSON

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

  Primitives.Number format info -> decodeNumber format info
  Primitives.Integer format info -> decodeInteger format info
  Primitives.String format -> decodeString format

encodePrimitive :: Primitives.Primitive a -> a -> Aeson.Value
encodePrimitive = \case
  Primitives.Boolean -> Aeson.toJSON
  Primitives.Number format _ -> encodeNumber format
  Primitives.Integer format _ -> encodeInteger format
  Primitives.String format -> encodeString format
