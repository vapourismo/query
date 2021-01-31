{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Utilities as Utilities
import           Data.Text (Text)
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics
import qualified Prettyprinter as Pretty
import qualified Type.Reflection as Reflection

-- | Shape of a field in a record
data FieldShapeF a = FieldShape
  { fieldShape_schema :: a
  -- ^ Shape of the field value
  , fieldShape_optional :: Bool
  -- ^ Is the field optional?
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving Show1 via Utilities.FunctorShow1 FieldShapeF

-- | Shape of something
data ShapeF a
  = Primitive Primitives.SomePrimitive
  | Nullable (ShapeF a)
  | Array a
  | StringMap a
  | Enum [Text]
  | Variant (HashMap Text a)
  | Record (HashMap Text (FieldShapeF a))
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
  deriving Show1 via Utilities.FunctorShow1 ShapeF

prettyShapeF :: ShapeF (Utilities.PrettyM ann) -> Utilities.PrettyM ann
prettyShapeF = \case
  Primitive prim -> pure $ Pretty.pretty prim
  Nullable inner -> do
    inner <- prettyShapeF inner
    pure $ Pretty.parens inner <> Pretty.pretty "?"
  Array inner -> Pretty.brackets <$> inner
  StringMap inner -> Pretty.braces <$> inner
  Enum names -> pure $ inAngles $ map Pretty.pretty names
  Variant constructors ->
    inAngles . map (uncurry (typed . Pretty.pretty)) . HashMap.toList <$> sequenceA constructors
  Record fields -> do
    fields <- for (HashMap.toList fields) $ \(name, FieldShape typ optional) -> do
      let questionMark = if optional then Pretty.pretty "?" else mempty
      typed (Pretty.pretty name <> questionMark) <$> typ
    pure $ inBraces fields
  where
    inAngles = Pretty.encloseSep Pretty.langle Pretty.rangle Pretty.comma

    inBraces = Pretty.encloseSep Pretty.lbrace Pretty.rbrace (Pretty.comma <> Pretty.space)

    typed name typ = name Pretty.<+> Pretty.colon Pretty.<+> typ

-- | Shape of something
type Shape = Fix ShapeF

-- | Make 'Shape' pretty.
prettyShape :: Shape -> Pretty.Doc ann
prettyShape = Utilities.runPrettyM . foldFix prettyShapeF

instance Pretty.Pretty Shape where
  pretty = prettyShape

-- | Shape of something queryable
data QueryShapeF a = QueryShape
  { queryShape_type :: Reflection.SomeTypeRep
  -- ^ Type of the thing to query
  , queryShape_shape :: Maybe (ShapeF a)
  -- ^ Optional shape of the thing to query for
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving Show1 via Utilities.FunctorShow1 QueryShapeF

prettyQueryShapeF :: QueryShapeF (Utilities.PrettyM ann) -> Utilities.PrettyM ann
prettyQueryShapeF (QueryShape typ shape) =
  Utilities.recursePretty typ $
    case shape of
      Nothing -> pure $ Pretty.pretty $ show typ
      Just shape -> prettyShapeF shape

-- | Shape of something queryable
type QueryShape = Fix QueryShapeF

-- | Make 'QueryShape' pretty.
prettyQueryShape :: QueryShape -> Pretty.Doc ann
prettyQueryShape = Utilities.runPrettyM . foldFix prettyQueryShapeF

instance Pretty.Pretty QueryShape where
  pretty = prettyQueryShape
