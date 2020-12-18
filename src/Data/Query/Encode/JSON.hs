{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Data.Query.Encode.JSON
  ( runEncoder
  )
where

import qualified Data.Aeson as Aeson
import           Data.Functor.Contravariant.Coyoneda (Coyoneda (Coyoneda))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Encode.Types as Types
import qualified Data.SOP as SOP
import qualified Data.Vector as Vector

runEncoder :: Types.Encoder a -> a -> Aeson.Value
runEncoder (Types.Encoder (Coyoneda f encoderBase)) =
  runEncoderBase encoderBase . f

runEncoderBase :: Types.EncoderBase a -> a -> Aeson.Value
runEncoderBase encoder value =
  case encoder of
    Types.BoolEncoder ->
      Aeson.Bool value

    Types.NumberEncoder ->
      Aeson.Number value

    Types.StringEncoder ->
      Aeson.String value

    Types.NullableEncoder encoder ->
      case value of
        Just value -> runEncoder encoder value
        Nothing    -> Aeson.Null

    Types.ArrayEncoder encoder ->
      Aeson.Array $ Vector.map (runEncoder encoder) value

    Types.StringMapEncoder encoder ->
      Aeson.Object $ HashMap.map (runEncoder encoder) value

    Types.EnumEncoder items ->
      SOP.hcollapse $
        SOP.hzipWith
          (\(Types.ItemEncoder value) _ -> SOP.K (Aeson.String value))
          items
          value

    Types.VariantEncoder constructors ->
      SOP.hcollapse $
        SOP.hzipWith
          (\(Types.ConstructorEncoder name encoder) ->
            SOP.K . Aeson.Object . HashMap.singleton name . runEncoder encoder
          )
          constructors
          value

    Types.RecordEncoder fields ->
      Aeson.Object $
        HashMap.mapMaybe
          (\case
            Types.FieldEncoder (Coyoneda f (Types.MandatoryFieldSelector encoder)) ->
              Just $ runEncoder encoder $ f value
            Types.FieldEncoder (Coyoneda f (Types.OptionalFieldSelector encoder)) ->
              runEncoder encoder <$> f value
          )
          fields

