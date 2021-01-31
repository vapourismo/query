{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Query.Primitives.OpenAPI3
  ( toSchema )
where

import qualified Data.OpenApi as OpenAPI
import qualified Data.Query.Primitives as Primitives

-- | Generate an OpenAPI 3 schema that represents the given primitive type.
toSchema :: Primitives.Primitive a -> OpenAPI.Schema
toSchema = \case
  Primitives.Boolean -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiBoolean }

  Primitives.Number info ->
    let
      format =
        case Primitives.numberInfo_format info of
          Primitives.NoNumberFormat -> Nothing
          Primitives.FloatFormat -> Just "float"
          Primitives.DoubleFormat -> Just "double"

      checkLimit limit =
        case Primitives.normalizeNumberLimit (Primitives.numberInfo_format info) limit of
          Primitives.NoLimit -> (Nothing, Nothing)
          Primitives.InclusiveLimit limit -> (Just limit, Just False)
          Primitives.ExclusiveLimit limit -> (Just limit, Just True)

      (upperLimit, exclusiveUpper) = checkLimit (Primitives.numberInfo_upperLimit info)
      (lowerLimit, exclusiveLower) = checkLimit (Primitives.numberInfo_lowerLimit info)

    in mempty
      { OpenAPI._schemaType = Just OpenAPI.OpenApiNumber
      , OpenAPI._schemaFormat = format
      , OpenAPI._schemaMinimum = lowerLimit
      , OpenAPI._schemaExclusiveMinimum = exclusiveLower
      , OpenAPI._schemaMaximum = upperLimit
      , OpenAPI._schemaExclusiveMaximum = exclusiveUpper
      }

  Primitives.Integer info ->
    let
      format =
        case Primitives.integerInfo_format info of
          Primitives.NoIntegerFormat -> Nothing
          Primitives.Int32Format -> Just "int32"
          Primitives.Int64Format -> Just "int64"

      checkLimit limit =
        case Primitives.normalizeIntegerLimit (Primitives.integerInfo_format info) limit of
          Primitives.NoLimit -> (Nothing, Nothing)
          Primitives.InclusiveLimit limit -> (Just limit, Just False)
          Primitives.ExclusiveLimit limit -> (Just limit, Just True)

      (upperLimit, exclusiveUpper) = checkLimit (Primitives.integerInfo_upperLimit info)
      (lowerLimit, exclusiveLower) = checkLimit (Primitives.integerInfo_lowerLimit info)

      multipleOf =
        Primitives.normalizeIntegerLimit
          (Primitives.integerInfo_format info)
          (Primitives.integerInfo_multipleOf info)

    in mempty
      { OpenAPI._schemaType = Just OpenAPI.OpenApiNumber
      , OpenAPI._schemaFormat = format
      , OpenAPI._schemaMinimum = lowerLimit
      , OpenAPI._schemaExclusiveMinimum = exclusiveLower
      , OpenAPI._schemaMaximum = upperLimit
      , OpenAPI._schemaExclusiveMaximum = exclusiveUpper
      , OpenAPI._schemaMultipleOf = multipleOf
      }

  Primitives.String format ->
    let
      schemaFormat =
        case format of
          Primitives.NoStringFormat -> Nothing
          Primitives.ByteFormat -> Just "byte"
          Primitives.BinaryFormat -> Just "binary"
          Primitives.DateFormat -> Just "date"
          Primitives.DateTimeFormat -> Just "date-time"
          Primitives.PasswordFormat -> Just "password"

    in mempty
      { OpenAPI._schemaType = Just OpenAPI.OpenApiString
      , OpenAPI._schemaFormat = schemaFormat
      }
