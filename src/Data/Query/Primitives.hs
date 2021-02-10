{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Query.Primitives
  ( SomePrimitive (..)
  , Primitive (..)
  , reifyPrimitiveConstraints
  , NumberInfo (..)
  , NumberFormat (..)
  , reifyNumberConstraints
  , normalizeNumberLimit
  , IntegerInfo (..)
  , IntegerFormat (..)
  , reifyIntegerConstraints
  , normalizeIntegerLimit
  , StringFormat (..)
  , reifyStringConstraints

  , Limit (..)
  )
where

import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import           Data.Int (Int32, Int64)
import           Data.Scientific (Scientific, fromFloatDigits)
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import qualified Prettyprinter as Pretty
import qualified Type.Reflection as Reflection

---

data Limit a
  = NoLimit
  | InclusiveLimit a
  | ExclusiveLimit a
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

---

data NumberFormat a where
  NoNumberFormat :: NumberFormat Scientific
  FloatFormat :: NumberFormat Float
  DoubleFormat :: NumberFormat Double

deriving instance Show (NumberFormat a)

deriving instance Eq (NumberFormat a)

deriving instance Ord (NumberFormat a)

reifyNumberConstraints :: NumberFormat a -> (NumberConstraint a => r) -> r
reifyNumberConstraints = \case
  NoNumberFormat -> id
  FloatFormat -> id
  DoubleFormat -> id

normalizeNumberLimit :: Functor f => NumberFormat a -> f a -> f Scientific
normalizeNumberLimit = \case
  NoNumberFormat -> id
  FloatFormat -> fmap fromFloatDigits
  DoubleFormat -> fmap fromFloatDigits

data NumberInfo a = NumberInfo
  { numberInfo_lowerLimit :: Limit a
  , numberInfo_upperLimit :: Limit a
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

type NumberConstraint a =
  (Aeson.FromJSON a, Aeson.ToJSON a, Show a, Ord a, Reflection.Typeable a)

---

data IntegerFormat a where
  NoIntegerFormat :: IntegerFormat Integer
  Int32Format :: IntegerFormat Int32
  Int64Format :: IntegerFormat Int64

deriving instance Show (IntegerFormat a)

deriving instance Eq (IntegerFormat a)

deriving instance Ord (IntegerFormat a)

reifyIntegerConstraints :: IntegerFormat a -> (IntegerConstraint a => r) -> r
reifyIntegerConstraints = \case
  NoIntegerFormat -> id
  Int32Format -> id
  Int64Format -> id

normalizeIntegerLimit :: Functor f => IntegerFormat a -> f a -> f Scientific
normalizeIntegerLimit = fmap . \case
  NoIntegerFormat -> fromIntegral
  Int32Format -> fromIntegral
  Int64Format -> fromIntegral

data IntegerInfo a = IntegerInfo
  { integerInfo_lowerLimit :: Limit a
  , integerInfo_upperLimit :: Limit a
  , integerInfo_multipleOf :: Maybe a
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

type IntegerConstraint a =
  (Aeson.FromJSON a, Aeson.ToJSON a, Show a, Integral a, Ord a, Reflection.Typeable a)

---

data StringFormat a where
  -- | No format
  NoStringFormat :: StringFormat Text

  -- | @byte@ format
  ByteFormat :: StringFormat ByteString

  -- | @binary@ format
  BinaryFormat :: StringFormat Text

  -- | @date@ format
  DateFormat :: StringFormat Day

  -- | @date-time@ format
  DateTimeFormat :: StringFormat UTCTime

  -- | @password@ format
  PasswordFormat :: StringFormat Text

deriving instance Show (StringFormat a)

deriving instance Eq (StringFormat a)

deriving instance Ord (StringFormat a)

type StringConstraints a = (Show a, Ord a, Reflection.Typeable a)

reifyStringConstraints :: StringFormat a -> (StringConstraints a => r) -> r
reifyStringConstraints = \case
  NoStringFormat -> id
  ByteFormat -> id
  BinaryFormat -> id
  DateFormat -> id
  DateTimeFormat -> id
  PasswordFormat -> id

---

data Primitive a where
  Boolean :: Primitive Bool
  Number :: NumberFormat a -> NumberInfo a -> Primitive a
  Integer :: IntegerFormat a -> IntegerInfo a -> Primitive a
  String :: StringFormat a -> Primitive a

deriving instance Show a => Show (Primitive a)

deriving instance Eq a => Eq (Primitive a)

deriving instance Ord a => Ord (Primitive a)

instance Pretty.Pretty (Primitive a) where
  pretty = \case
    Boolean{} -> Pretty.pretty @String "Boolean"
    Number{} -> Pretty.pretty @String "Number"
    Integer{} -> Pretty.pretty @String "Integer"
    String{} -> Pretty.pretty @String "String"

type PrimitiveConstraints a = (Show a, Ord a, Reflection.Typeable a)

reifyPrimitiveConstraints :: Primitive a -> (PrimitiveConstraints a => r) -> r
reifyPrimitiveConstraints = \case
  Boolean -> id
  Number format _ -> reifyNumberConstraints format
  Integer format _ -> reifyIntegerConstraints format
  String format -> reifyStringConstraints format

data SomePrimitive where
  SomePrimitive :: Primitive a -> SomePrimitive

instance Show SomePrimitive where
  showsPrec prec (SomePrimitive primitive) =
    reifyPrimitiveConstraints primitive showsPrec prec primitive

instance Eq SomePrimitive where
  SomePrimitive (lhs :: Primitive lhs) == SomePrimitive (rhs :: Primitive rhs) =
    reifyPrimitiveConstraints lhs $ reifyPrimitiveConstraints rhs $
      case Reflection.eqTypeRep (Reflection.typeRep @lhs) (Reflection.typeRep @rhs) of
        Just Reflection.HRefl -> lhs == rhs
        Nothing -> False

instance Ord SomePrimitive where
  compare (SomePrimitive (lhs :: Primitive lhs)) (SomePrimitive (rhs :: Primitive rhs)) =
    reifyPrimitiveConstraints lhs $ reifyPrimitiveConstraints rhs $
      let
        leftRep = Reflection.typeRep @lhs
        rightRep = Reflection.typeRep @rhs
      in
        case Reflection.eqTypeRep leftRep rightRep of
          Just Reflection.HRefl -> compare lhs rhs
          Nothing -> compare (Reflection.SomeTypeRep leftRep) (Reflection.SomeTypeRep rightRep)

instance Pretty.Pretty SomePrimitive where
  pretty (SomePrimitive prim) = Pretty.pretty prim
