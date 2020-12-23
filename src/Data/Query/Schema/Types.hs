{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Schema.Types where

import           Control.Applicative.Free (Ap)
import           Data.HashMap.Strict (HashMap)
import           Data.Profunctor (Profunctor (..))
import           Data.Profunctor.Yoneda (Coyoneda (..))
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics
import qualified Type.Reflection as Reflection

-- | Schema for a variant constructor
data ConstructorSchema f a where
  ConstructorSchema
    :: Text
    -- ^ Constructor name
    -> Coyoneda QuerySchema (f a) (f a)
    -- ^ Constructor value schema
    -> ConstructorSchema f a

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

-- | Schema field composition; you can interpret this as a record schema for an encodable type @a@
-- or a decodable type @b@.
newtype FieldsSchema a b = FieldsSchema
  { unFieldsSchema :: Ap (Coyoneda FieldSchema a) b }
  deriving newtype (Functor, Applicative)
  deriving Profunctor via Utilities.ApCoyoneda FieldSchema

data ItemSchema f a = ItemSchema
  -- ^ Enum item
  { itemSchema_identifier :: Text
  -- ^ Enum value identifier
  , itemSchema_value :: f a
  -- ^ Value representing the enum value
  }

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

-- | Schema for encoding @a@ and decoding @b@
newtype Schema a b = Schema
  { unSchema :: Coyoneda SchemaBase a b }
  deriving newtype Profunctor

data QuerySchema a b
  = DecodableSchema
    -- ^ Query for things that can be encoded and decoded
    { querySchema_decodeType :: Reflection.TypeRep b
    -- ^ Type of the thing to be decoded
    , querySchema_schema :: Schema a b
    -- ^ Schema for the things
    }
  | UndecodableSchema
    -- ^ Query for something that can't be decoded
    { querySchema_decodeType :: Reflection.TypeRep b
    -- ^ Type of the thing to be decoded
    , querySchema_encode :: Encode.Encoder a
    -- ^ Encoder for the thing
    }

data FieldShape = FieldShape
  { fieldShape_schema :: Shape
  -- ^ Shape of the field value
  , fieldShape_optional :: Bool
  -- ^ Is the field optional?
  }
  deriving stock (Show, Generic)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)

deriving
  via
    Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] FieldShape
  instance
    Encode.HasFieldsEncoder FieldShape

deriving
  via
    Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] FieldShape
  instance
    Decode.HasFieldsDecoder FieldShape

instance Encode.HasEncoder FieldShape where
  encoder = Encode.record

instance Decode.HasDecoder FieldShape where
  decoder = Decode.record

data Shape
  = Bool
  | Number
  | String
  | Nullable Shape
  | Array Shape
  | StringMap Shape
  | Enum [Text]
  | Variant (HashMap Text Shape)
  | Record (HashMap Text FieldShape)
  deriving stock (Show, Generic)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving (Encode.HasEncoder, Decode.HasDecoder) via Generic.Generic Shape

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
