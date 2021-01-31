{-# LANGUAGE LambdaCase #-}

module Data.Query.Shape.OpenAPI3
  ( toSchema )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.OpenApi as OpenAPI
import           Data.Query.Primitives (SomePrimitive (SomePrimitive))
import qualified Data.Query.Primitives.OpenAPI3 as Primitives
import qualified Data.Query.Shape as Shape
import           GHC.Exts (IsList (fromList))

toSchema :: Shape.ShapeF (OpenAPI.Referenced OpenAPI.Schema) -> OpenAPI.Schema
toSchema = \case
  Shape.Primitive (SomePrimitive prim) ->
    Primitives.toSchema prim

  Shape.Nullable shape ->
    (toSchema shape) { OpenAPI._schemaNullable = Just True }

  Shape.Array ref -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiArray
    , OpenAPI._schemaItems = Just (OpenAPI.OpenApiItemsObject ref)
    }

  Shape.StringMap ref -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiObject
    , OpenAPI._schemaAdditionalProperties = Just $ OpenAPI.AdditionalPropertiesSchema ref
    }

  Shape.Enum values -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiString
    , OpenAPI._schemaEnum = Just $ map Aeson.String values
    }

  Shape.Variant constructors -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiObject
    , OpenAPI._schemaProperties = fromList $ HashMap.toList constructors
    }

  Shape.Record fields ->
    let
      properties = fromList $ HashMap.toList $ HashMap.map Shape.fieldShape_schema fields

      required =
        [ name
        | (name, field) <- HashMap.toList fields
        , not (Shape.fieldShape_optional field)
        ]

    in mempty
      { OpenAPI._schemaType = Just OpenAPI.OpenApiObject
      , OpenAPI._schemaProperties = properties
      , OpenAPI._schemaRequired = required
      }
