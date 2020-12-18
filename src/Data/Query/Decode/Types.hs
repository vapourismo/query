{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Decode.Types where

import           Control.Applicative.Free (Ap, runAp_)
import           Data.Functor.Coyoneda (Coyoneda (..))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.List as List
import qualified Data.Query.Utilities as Utilities
import qualified Data.Query.Value as Value
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Type.Reflection as Reflection

data Query a where
  DecodableQuery
    :: Reflection.TypeRep a
    -> Decoder a
    -> Query a

  UndecodableQuery
    :: Reflection.TypeRep a
    -> Query a

instance Show (Query a) where
  show = \case
    DecodableQuery typ _decoder -> show typ
    UndecodableQuery typ -> show typ

newtype ConstructorQuery a = ConstructorQuery
  { unConstructorQuery :: Coyoneda Query a }
  deriving newtype Functor
  deriving Show via Utilities.CoyonedaShow Query a

data FieldQuery a where
  MandatoryFieldQuery
    :: Text
    -> Query a
    -> FieldQuery a

  OptionalFieldQuery
    :: Text
    -> Query a
    -> FieldQuery (Maybe a)

instance Show (FieldQuery a) where
  show = \case
    MandatoryFieldQuery name query -> Text.unpack name <> " : " <> show query
    OptionalFieldQuery name query -> Text.unpack name <> "? : " <> show query

newtype FieldsDecoder a = FieldsDecoder
  { unFieldDecoder :: Ap FieldQuery a }
  deriving newtype (Functor, Applicative)

instance Show (FieldsDecoder a) where
  show (FieldsDecoder fields) = "{" <> List.intercalate ", " (runAp_ (pure . show) fields) <> "}"

data DecoderBase a where
  BoolDecoder
    :: DecoderBase Bool

  NumberDecoder
    :: DecoderBase Scientific

  StringDecoder
    :: DecoderBase Text

  NullableDecoder
    :: Decoder a
    -> DecoderBase (Maybe a)

  ArrayDecoder
    :: Query a
    -> DecoderBase (Vector a)

  StringMapDecoder
    :: Query a
    -> DecoderBase (HashMap.HashMap Text a)

  EnumDecoder
    :: HashMap.HashMap Text a
    -> DecoderBase a

  VariantDecoder
    :: HashMap.HashMap Text (ConstructorQuery a)
    -> DecoderBase a

  RecordDecoder
    :: FieldsDecoder a
    -> DecoderBase a

instance Show (DecoderBase a) where
  show = \case
    BoolDecoder -> "Bool"
    NumberDecoder -> "Number"
    StringDecoder -> "String"
    NullableDecoder encoder -> "(" <> show encoder <> ")?"
    ArrayDecoder encoder -> "[" <> show encoder <> "]"
    StringMapDecoder encoder -> "{String => " <> show encoder <> "}"
    EnumDecoder decoders -> "<" <> List.intercalate " | " (map Text.unpack (HashMap.keys decoders)) <> ">"
    VariantDecoder decoders -> "<" <> List.intercalate " | " (map (\(key, encoder) -> Text.unpack key <> " : " <> show encoder) (HashMap.toList decoders)) <> ">"
    RecordDecoder fields -> show fields

newtype Decoder a = Decoder
  { unDecoder :: Coyoneda DecoderBase a }
  deriving newtype Functor
  deriving Show via Utilities.CoyonedaShow DecoderBase a

data ConstructorMatch a = ConstructorMatch Text (ConstructorQuery a) Value.Value

deriving instance Show (ConstructorMatch a)

data DecodeError
  = forall a. UnexpectedInput (DecoderBase a) Value.NoCallValue
  | forall a. UndecodableNeedsCall (Reflection.TypeRep a) Value.NoCallValue
  | MissingField Text
  | UnknownEnum (HashSet Text) Text
  | NoMatchingConstructor (HashSet Text) Value.Object
  | forall a. MultipleConstructors [ConstructorMatch a]

deriving instance Show DecodeError
