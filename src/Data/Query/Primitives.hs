{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Query.Primitives
  ( Primitive (..)
  , SomePrimitive (..)
  , NumberFormat (..)
  , IntegerFormat (..)
  , StringFormat (..)
  , encodePrimitive
  , decodePrimitive
  )
where

import qualified Codec.Base64 as Base64
import           Control.Monad (unless, when, (<=<))
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import           Data.Int (Int32, Int64)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import qualified Type.Reflection as Reflection

---

data Limit a
  = NoLimit
  | InclusiveLimit a
  | ExclusiveLimit a
  deriving (Show, Eq, Ord)

---

data NumberFormat a where
  NoNumberFormat :: NumberFormat Scientific
  FloatFormat :: NumberFormat Float
  DoubleFormat :: NumberFormat Double

deriving instance Show (NumberFormat a)

deriving instance Eq (NumberFormat a)

deriving instance Ord (NumberFormat a)

reifyNumberConstraints :: NumberFormat a -> (NumberParseConstraint a => r) -> r
reifyNumberConstraints = \case
  NoNumberFormat -> id
  FloatFormat -> id
  DoubleFormat -> id

data NumberInfo a = NumberInfo
  { numberInfo_format :: NumberFormat a
  , numberInfo_lowerLimit :: Limit a
  , numberInfo_upperLimit :: Limit a
  }
  deriving (Show, Eq, Ord)

type NumberParseConstraint a =
  (Aeson.FromJSON a, Aeson.ToJSON a, Show a, Ord a, Reflection.Typeable a)

parseNumber :: NumberInfo a -> Aeson.Value -> Aeson.Parser a
parseNumber info value = reifyNumberConstraints (numberInfo_format info) $ do
  number <- Aeson.parseJSON value

  case numberInfo_lowerLimit info of
    NoLimit -> pure ()
    InclusiveLimit limit -> unless (number >= limit) $ fail $ show number <> " < " <> show limit
    ExclusiveLimit limit -> unless (number > limit) $ fail $ show number <> " <= " <> show limit

  case numberInfo_upperLimit info of
    NoLimit -> pure ()
    InclusiveLimit limit -> unless (number <= limit) $ fail $ show number <> " > " <> show limit
    ExclusiveLimit limit -> unless (number < limit) $ fail $ show number <> " >= " <> show limit

  pure number

encodeNumber :: NumberInfo a -> a -> Aeson.Value
encodeNumber info = reifyNumberConstraints (numberInfo_format info) Aeson.toJSON

---

data IntegerFormat a where
  NoIntegerFormat :: IntegerFormat Integer
  Int32Format :: IntegerFormat Int32
  Int64Format :: IntegerFormat Int64

deriving instance Show (IntegerFormat a)

deriving instance Eq (IntegerFormat a)

deriving instance Ord (IntegerFormat a)

reifyIntegerConstraints :: IntegerFormat a -> (IntegerParseConstraint a => r) -> r
reifyIntegerConstraints = \case
  NoIntegerFormat -> id
  Int32Format -> id
  Int64Format -> id

data IntegerInfo a = IntegerInfo
  { integerInfo_format :: IntegerFormat a
  , integerInfo_lowerLimit :: Limit a
  , integerInfo_upperLimit :: Limit a
  , integerInfo_multipleOf :: Maybe a
  }
  deriving (Show, Eq, Ord)

type IntegerParseConstraint a =
  (Aeson.FromJSON a, Aeson.ToJSON a, Show a, Integral a, Ord a, Reflection.Typeable a)

parseInteger :: IntegerInfo a -> Aeson.Value -> Aeson.Parser a
parseInteger info value = reifyIntegerConstraints (integerInfo_format info) $ do
  integer <- Aeson.parseJSON value

  case integerInfo_lowerLimit info of
    NoLimit -> pure ()
    InclusiveLimit limit -> unless (integer >= limit) $ fail $ show integer <> " < " <> show limit
    ExclusiveLimit limit -> unless (integer > limit) $ fail $ show integer <> " <= " <> show limit

  case integerInfo_upperLimit info of
    NoLimit -> pure ()
    InclusiveLimit limit -> unless (integer <= limit) $ fail $ show integer <> " > " <> show limit
    ExclusiveLimit limit -> unless (integer < limit) $ fail $ show integer <> " >= " <> show limit

  case integerInfo_multipleOf info of
    Just multiple -> when (mod integer multiple /= 0) $ fail ""
    Nothing -> pure ()

  pure integer

encodeInteger :: IntegerInfo a -> a -> Aeson.Value
encodeInteger info = reifyIntegerConstraints (integerInfo_format info) Aeson.toJSON

---

data StringFormat a where
  NoStringFormat :: StringFormat Text
  ByteFormat :: StringFormat ByteString
  BinaryFormat :: StringFormat Text
  DateFormat :: StringFormat Day
  DateTimeFormat :: StringFormat UTCTime
  PasswordFormat :: StringFormat Text

deriving instance Show (StringFormat a)

deriving instance Eq (StringFormat a)

deriving instance Ord (StringFormat a)

parseString :: StringFormat a -> Aeson.Value -> Aeson.Parser a
parseString = \case
  NoStringFormat -> Aeson.parseJSON
  ByteFormat -> either fail pure . Base64.decode @Text <=< Aeson.parseJSON
  BinaryFormat -> Aeson.parseJSON
  DateFormat -> Aeson.parseJSON
  DateTimeFormat -> Aeson.parseJSON
  PasswordFormat   -> Aeson.parseJSON

encodeString :: StringFormat a -> a -> Aeson.Value
encodeString = \case
  NoStringFormat -> Aeson.toJSON
  ByteFormat -> Aeson.toJSON @Text . Base64.encode
  BinaryFormat -> Aeson.toJSON
  DateFormat -> Aeson.toJSON
  DateTimeFormat -> Aeson.toJSON
  PasswordFormat  -> Aeson.toJSON

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
  Number :: NumberInfo a -> Primitive a
  Integer :: IntegerInfo a -> Primitive a
  String :: StringFormat a -> Primitive a

deriving instance Show a => Show (Primitive a)

deriving instance Eq a => Eq (Primitive a)

deriving instance Ord a => Ord (Primitive a)

decodePrimitive :: Primitive a -> Aeson.Value -> Aeson.Parser a
decodePrimitive = \case
  Boolean -> Aeson.parseJSON
  Number info -> parseNumber info
  Integer info -> parseInteger info
  String format -> parseString format

encodePrimitive :: Primitive a -> a -> Aeson.Value
encodePrimitive = \case
  Boolean -> Aeson.toJSON
  Number info -> encodeNumber info
  Integer info -> encodeInteger info
  String format -> encodeString format

type PrimitiveConstraints a = (Show a, Ord a, Reflection.Typeable a)

reifyPrimitiveConstraints :: Primitive a -> (PrimitiveConstraints a => r) -> r
reifyPrimitiveConstraints = \case
  Boolean -> id
  Number info -> reifyNumberConstraints (numberInfo_format info)
  Integer info -> reifyIntegerConstraints (integerInfo_format info)
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
