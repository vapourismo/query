{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Query.Shape
  ( FieldShapeF (..)
  , ShapeF (..)
  , prettyShapeF
  , Shape
  , prettyShape
  , QueryShapeF (..)
  , prettyQueryShapeF
  , QueryShape
  , prettyQueryShape
  )
where

import           Data.Fix (Fix (..), foldFix)
import           Data.Functor.Classes (Show1 (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema as Schema
import qualified Data.Query.Utilities as Utilities
import           Data.Text (Text)
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics
import qualified Prettyprinter as Pretty
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
  | Nullable (ShapeF a)
  | Array a
  | StringMap a
  | Enum [Text]
  | Variant (HashMap Text a)
  | Record (HashMap Text (FieldShapeF a))
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving (Encode.HasEncoder, Decode.HasDecoder, Schema.HasSchema) via Generic.Generic (ShapeF a)
  deriving Show1 via Utilities.FunctorShow1 ShapeF

prettyShapeF :: ShapeF (Utilities.PrettyM ann) -> Utilities.PrettyM ann
prettyShapeF = \case
  Bool -> pure $ Pretty.pretty "Bool"
  Number -> pure $ Pretty.pretty "Number"
  String -> pure $ Pretty.pretty "String"
  Nullable inner -> do
    inner <- prettyShapeF inner
    pure $ Pretty.parens inner <> Pretty.pretty "?"
  Array inner -> Pretty.brackets <$> inner
  StringMap inner -> Pretty.braces <$> inner
  Enum names -> pure $ inAngles $ map Pretty.pretty names
  Variant constructors ->
    inAngles . map (uncurry (typed . Pretty.pretty)) . HashMap.toList <$> sequenceA constructors
  Record fields -> do
    fields <- for (HashMap.toList fields) $ \(name, FieldShapeF typ optional) -> do
      let questionMark = if optional then Pretty.pretty "?" else mempty
      typed (Pretty.pretty name <> questionMark) <$> typ
    pure $ inBraces fields
  where
    inAngles = Pretty.encloseSep Pretty.langle Pretty.rangle Pretty.comma

    inBraces = Pretty.encloseSep Pretty.lbrace Pretty.rbrace Pretty.comma

    typed name typ = name Pretty.<+> Pretty.colon Pretty.<+> typ

type Shape = Fix ShapeF

prettyShape :: Shape -> Pretty.Doc ann
prettyShape = Utilities.runPrettyM . foldFix prettyShapeF

data QueryShapeF a = QueryShape
  { queryShape_type :: Reflection.SomeTypeRep
  , queryShape_shape :: Maybe (ShapeF a)
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving Show1 via Utilities.FunctorShow1 QueryShapeF

prettyQueryShapeF :: QueryShapeF (Utilities.PrettyM ann) -> Utilities.PrettyM ann
prettyQueryShapeF (QueryShape typ shape) =
  Utilities.recursePretty typ $
    case shape of
      Nothing -> pure $ Pretty.pretty $ show typ
      Just shape -> prettyShapeF shape

type QueryShape = Fix QueryShapeF

prettyQueryShape :: QueryShape -> Pretty.Doc ann
prettyQueryShape = Utilities.runPrettyM . foldFix prettyQueryShapeF
