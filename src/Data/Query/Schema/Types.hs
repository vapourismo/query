{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Schema.Types where

import           Control.Applicative.Free (Ap, runAp_)
import           Data.Fix (Fix (Fix), foldFix)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Profunctor (Profunctor (..))
import           Data.Profunctor.Yoneda (Coyoneda (..))
import qualified Data.Query.Encode.Types as Encode
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics
import qualified Prettyprinter as Pretty
import qualified Type.Reflection as Reflection

-- | Schema for a variant constructor
data ConstructorSchema f a = ConstructorSchema
  { constructorSchema_name :: Text
    -- ^ Constructor name
  , constructorSchema_schema :: Coyoneda QuerySchema (f a) (f a)
    -- ^ Constructor value schema
  }

-- | Schema of a field of type @b@ from a record @a@
data FieldSchema a b where
  MandatoryFieldSchema
    :: Text
    -- ^ Field name
    -> QuerySchema a b
    -- ^ Field schema
    -> FieldSchema a b

  OptionalFieldSchema
    :: Text
    -- ^ Field name
    -> QuerySchema a b
    -- ^ Field schema
    -> FieldSchema (Maybe a) (Maybe b)

deriving instance Show (FieldSchema a b)

-- | Schema field composition; you can interpret this as a record schema for an encodable type @a@
-- or a decodable type @b@.
newtype FieldsSchema a b = FieldsSchema
  { unFieldsSchema :: Ap (Coyoneda FieldSchema a) b }
  deriving newtype (Functor, Applicative)
  deriving Profunctor via Utilities.ApCoyoneda FieldSchema

instance Show (FieldsSchema a b) where
  show = show . Pretty.pretty

instance Pretty.Pretty (FieldsSchema a b) where
  pretty =
    Utilities.runPrettyM
    . Shape.prettyShapeF
    . fmap (foldFix Shape.prettyQueryShapeF)
    . Shape.Record
    . fieldsSchemaToFieldShapes

fieldsSchemaToFieldShapes
  :: FieldsSchema a b
  -> HashMap.HashMap Text (Shape.FieldShapeF Shape.QueryShape )
fieldsSchemaToFieldShapes (FieldsSchema body) =
  runAp_
    (\(Coyoneda _ _ schema) ->
      case schema of
        MandatoryFieldSchema name schema -> HashMap.singleton name Shape.FieldShape
          { Shape.fieldShape_schema = querySchemaToQueryShape schema
          , Shape.fieldShape_optional = False
          }

        OptionalFieldSchema name schema -> HashMap.singleton name Shape.FieldShape
          { Shape.fieldShape_schema = querySchemaToQueryShape schema
          , Shape.fieldShape_optional = True
          }
    )
    body

-- | Enum item
data ItemSchema f a = ItemSchema
  { itemSchema_name :: Text
  -- ^ Enum value identifier
  , itemSchema_value :: f a
  -- ^ Value representing the enum value
  }
  deriving Show

data SchemaBase a b where
  BoolSchema
    :: SchemaBase Bool Bool

  NumberSchema
    :: SchemaBase Scientific Scientific

  StringSchema
    :: SchemaBase Text Text

  NullableSchema
    :: Schema a b
    -> SchemaBase (Maybe a) (Maybe b)

  ArraySchema
    :: QuerySchema a b
    -> SchemaBase (Vector a) (Vector b)

  StringMapSchema
    :: QuerySchema a b
    -> SchemaBase (HashMap Text a) (HashMap Text b)

  EnumSchema
    :: SOP.SListI xs
    => SOP.NP (ItemSchema f) xs
    -> SchemaBase (SOP.NS f xs) (SOP.NS f xs)

  VariantSchema
    :: SOP.SListI xs
    => SOP.NP (ConstructorSchema f) xs
    -> SchemaBase (SOP.NS f xs) (SOP.NS f xs)

  RecordSchema
    :: FieldsSchema a b
    -> SchemaBase a b

instance Show (SchemaBase a b) where
  show = show . Pretty.pretty

instance Pretty.Pretty (SchemaBase a b) where
  pretty =
    Utilities.runPrettyM
    . Shape.prettyShapeF
    . fmap (foldFix Shape.prettyQueryShapeF)
    . schemaBaseToShape

schemaBaseToShape :: SchemaBase a b -> Shape.ShapeF Shape.QueryShape
schemaBaseToShape = \case
  BoolSchema -> Shape.Bool
  NumberSchema -> Shape.Number
  StringSchema -> Shape.String
  NullableSchema schema -> Shape.Nullable $ schemaToShape schema
  ArraySchema query -> Shape.Array $ querySchemaToQueryShape query
  StringMapSchema query -> Shape.StringMap $ querySchemaToQueryShape query
  EnumSchema items -> Shape.Enum $ SOP.hcollapse $ SOP.hmap (SOP.K . itemSchema_name) items
  VariantSchema constructors ->
    Shape.Variant
    $ HashMap.fromList
    $ SOP.hcollapse
    $ SOP.hmap
        (\(ConstructorSchema name (Coyoneda _ _ schema)) ->
          SOP.K (name, querySchemaToQueryShape schema)
        )
        constructors
  RecordSchema fields -> Shape.Record $ fieldsSchemaToFieldShapes fields

-- | Schema for encoding @a@ and decoding @b@
newtype Schema a b = Schema
  { unSchema :: Coyoneda SchemaBase a b }
  deriving newtype Profunctor
  deriving (Show, Pretty.Pretty) via Utilities.ProfunctorCoyonedaShow SchemaBase a b

schemaToShape :: Schema a b -> Shape.ShapeF Shape.QueryShape
schemaToShape (Schema (Coyoneda _ _ base)) = schemaBaseToShape base

-- | Query for things that can be encoded and decoded
data QuerySchema a b = QuerySchema
  { querySchema_type :: Reflection.TypeRep b
  -- ^ Type of the thing to be decoded
  , querySchema_schema :: Either (Encode.Encoder a) (Schema a b)
  -- ^ Schema for undecodable or decodable @a@
  }
  deriving Show

instance Pretty.Pretty (QuerySchema a b) where
  pretty = Shape.prettyQueryShape . querySchemaToQueryShape

querySchemaToQueryShape :: QuerySchema a b -> Shape.QueryShape
querySchemaToQueryShape query = Fix Shape.QueryShape
  { Shape.queryShape_type = Reflection.SomeTypeRep $ querySchema_type query
  , Shape.queryShape_shape =
      either (const Nothing) (Just . schemaToShape) $ querySchema_schema query
  }

-- | Path into an encoded 'Data.Query.Value.Value', 'Schema', 'Data.Query.Decode.Types.Decoder'
-- or 'Data.Query.Encode.Types.Encoder'
data Path
  = ArrayPath
  -- ^ Items of an array
  | StringMapPath
  -- ^ Mapping value of a string map
  | ConstructorPath Text
  -- ^ Variant constructor body
  | FieldPath Text
  -- ^ Record field
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
