{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Query.Shape where

import           Data.Fix (Fix (..))
import           Data.Functor.Classes (Show1 (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema as Schema
import qualified Data.Query.Utilities as Utilities
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics
import qualified Type.Reflection as Reflection

data FieldShapeF a = FieldShapeF
  { fieldShape_schema :: a
  -- ^ Shape of the field value
  , fieldShape_optional :: Bool
  -- ^ Is the field optional?
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving Show1 via Utilities.FunctorShow1 FieldShapeF

type FieldShape = FieldShapeF Shape

deriving
  via
    Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (FieldShapeF a)
  instance
    Encode.HasEncoder a => Encode.HasFieldsEncoder (FieldShapeF a)

deriving
  via
    Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (FieldShapeF a)
  instance
    Decode.HasDecoder a => Decode.HasFieldsDecoder (FieldShapeF a)

deriving
  via
    Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (FieldShapeF a)
  instance
    Schema.HasSchema a => Schema.HasFieldsSchema (FieldShapeF a)

instance Encode.HasEncoder a => Encode.HasEncoder (FieldShapeF a) where
  encoder = Encode.record

instance Decode.HasDecoder a => Decode.HasDecoder (FieldShapeF a) where
  decoder = Decode.record

instance Schema.HasSchema a => Schema.HasSchema (FieldShapeF a) where
  schema = Schema.record

data ShapeF a
  = Bool
  | Number
  | String
  | Nullable a
  | Array a
  | StringMap a
  | Enum [Text]
  | Variant (HashMap Text a)
  | Record (HashMap Text (FieldShapeF a))
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving (Encode.HasEncoder, Decode.HasDecoder, Schema.HasSchema) via Generic.Generic (ShapeF a)
  deriving Show1 via Utilities.FunctorShow1 ShapeF

type Shape = Fix ShapeF

data QueryShapeF a = QueryShape
  { queryShape_type :: Reflection.SomeTypeRep
  , queryShape_shape :: Maybe (ShapeF a)
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving Show1 via Utilities.FunctorShow1 QueryShapeF

type QueryShape = Fix QueryShapeF
