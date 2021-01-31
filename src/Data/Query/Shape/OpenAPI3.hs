{-# LANGUAGE LambdaCase #-}

module Data.Query.Shape.OpenAPI3
  ( shapeToSchema
  , queryShapeToSchema
  )
where

import qualified Data.Aeson as Aeson
import           Data.Fix (Fix (..), foldFix)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.OpenApi as OpenAPI
import           Data.Query.Primitives (SomePrimitive (SomePrimitive))
import qualified Data.Query.Primitives.OpenAPI3 as Primitives
import qualified Data.Query.Shape as Shape
import qualified Data.Text as Text
import           GHC.Exts (IsList (fromList))

foldShapeF
  :: Shape.ShapeF (OpenAPI.Referenced OpenAPI.Schema)
  -> OpenAPI.Schema
foldShapeF = \case
  Shape.Primitive (SomePrimitive prim) ->
    Primitives.toSchema prim

  Shape.Nullable shape ->
    (foldShapeF shape) { OpenAPI._schemaNullable = Just True }

  Shape.Array ref -> mempty
    { OpenAPI._schemaType = Just OpenAPI.OpenApiArray
    , OpenAPI._schemaItems = Just $ OpenAPI.OpenApiItemsObject ref
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

shapeToSchema :: Shape.Shape -> OpenAPI.Referenced OpenAPI.Schema
shapeToSchema = foldFix (OpenAPI.Inline . foldShapeF)

queryShapeToSchema :: Shape.QueryShape -> OpenAPI.Schema
queryShapeToSchema (Fix queryShape) =
  case Shape.queryShape_shape queryShape of
    Just shape -> foldShapeF $ fmap typeRep shape
    Nothing -> mempty
  where
    typeRep = OpenAPI.Ref . OpenAPI.Reference . Text.pack . show . Shape.queryShape_type . unFix
