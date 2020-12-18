{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Query.Structure where

import           Control.Applicative.Free (Ap)
import           Data.HashMap.Strict (HashMap)
import           Data.Profunctor (Profunctor)
import           Data.Profunctor.Yoneda (Coyoneda)
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Vector (Vector)

data ItemStructure f a = ItemStructure
  { itemStructure_identifier :: Text
  , itemStructure_value      :: f a
  }

data ConstructorStructure p f a where
  ConstructorStructure
    :: Text
    -> (f a -> b)
    -> (c -> f a)
    -> p b c
    -> ConstructorStructure p f a

data FieldStructure p a b where
  MandatoryFieldStructure
    :: Text
    -> p a b
    -> FieldStructure p a b

  OptionalFieldStructure
    :: Text
    -> p a b
    -> FieldStructure p (Maybe a) (Maybe b)

newtype FieldsStructure p a b = FieldsStructure
  { unFieldsStructure :: Ap (Coyoneda (FieldStructure p) a) b }
  deriving newtype (Functor, Applicative)
  deriving Profunctor via Utilities.ApCoyoneda (FieldStructure p)

data StructureBase p a b where
  Bool
    :: StructureBase p Bool Bool

  Number
    :: StructureBase p Scientific Scientific

  String
    :: StructureBase p Text Text

  Nullable
    :: StructureBase p a b
    -> StructureBase p (Maybe a) (Maybe b)

  Array
    :: p a b
    -> StructureBase p (Vector a) (Vector b)

  StringMap
    :: p a b
    -> StructureBase p (HashMap Text a) (HashMap Text b)

  Enum
    :: SOP.SListI xs
    => SOP.NP (ItemStructure f) xs
    -> StructureBase p (SOP.NS f xs) (SOP.NS f xs)

  Variant
    :: SOP.SListI xs
    => SOP.NP (ConstructorStructure p f) xs
    -> StructureBase p (SOP.NS f xs) (SOP.NS f xs)

  Record
    :: FieldsStructure p a b
    -> StructureBase p a b

newtype Structure p a b = Structure
  { unStructure :: Coyoneda (StructureBase p) a b }
  deriving newtype Profunctor
