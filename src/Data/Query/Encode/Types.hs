{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Encode.Types where

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Coyoneda (Coyoneda)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)

newtype ItemEncoder a = ItemEncoder
  { unItemEncoder :: Text }

data ConstructorEncoder f a = ConstructorEncoder Text (Encoder (f a))

instance Functor f => Contravariant (ConstructorEncoder f) where
  contramap f (ConstructorEncoder name encoder) =
    ConstructorEncoder name (contramap (fmap f) encoder)

data FieldSelector a where
  MandatoryFieldSelector
    :: Encoder a
    -> FieldSelector a

  OptionalFieldSelector
    :: Encoder a
    -> FieldSelector (Maybe a)

instance Show (FieldSelector a) where
  show = \case
    MandatoryFieldSelector encoder -> "? : " <> show encoder
    OptionalFieldSelector encoder  -> " : " <> show encoder

newtype FieldEncoder a = FieldEncoder
  { unFieldEncoder :: Coyoneda FieldSelector a }
  deriving newtype Contravariant
  deriving Show via Utilities.ContravariantCoyonedaShow FieldSelector a

data EncoderBase a where
  BoolEncoder
    :: EncoderBase Bool

  NumberEncoder
    :: EncoderBase Scientific

  StringEncoder
    :: EncoderBase Text

  NullableEncoder
    :: Encoder a
    -> EncoderBase (Maybe a)

  ArrayEncoder
    :: Encoder a
    -> EncoderBase (Vector a)

  StringMapEncoder
    :: Encoder a
    -> EncoderBase (HashMap.HashMap Text a)

  EnumEncoder
    :: SOP.SListI xs
    => SOP.NP ItemEncoder xs
    -> EncoderBase (SOP.NS f xs)

  VariantEncoder
    :: SOP.SListI xs
    => SOP.NP (ConstructorEncoder f) xs
    -> EncoderBase (SOP.NS f xs)

  RecordEncoder
    :: HashMap.HashMap Text (FieldEncoder a)
    -> EncoderBase a

instance Show (EncoderBase a) where
  show = \case
    BoolEncoder -> "Bool"
    NumberEncoder -> "Number"
    StringEncoder -> "String"
    NullableEncoder encoder -> "(" <> show encoder <> ")?"
    ArrayEncoder encoder -> "[" <> show encoder <> "]"
    StringMapEncoder encoder -> "{ * : " <> show encoder <> " }"
    EnumEncoder encoders -> concat
      [ "< "
      , List.intercalate
          " | "
          (map
            Text.unpack
            (SOP.hcollapse (SOP.hmap (SOP.K . unItemEncoder) encoders))
          )
      , " >"
      ]
    VariantEncoder encoders -> concat
      [ "< "
      , List.intercalate
          " | "
          (map
            (\(key, encoder) -> Text.unpack key <> " : " <> encoder)
            (SOP.hcollapse
              (SOP.hmap
                (\(ConstructorEncoder name encoder) -> SOP.K (name, show encoder))
                encoders
              )
            )
          )
      , " >"
      ]
    RecordEncoder fields -> concat
      [ "{ "
      , List.intercalate
          ", "
          (map (\(key, encoder) -> Text.unpack key <> show encoder) (HashMap.toList fields))
      , " }"
      ]

newtype Encoder a = Encoder
  { unEncoder :: Coyoneda EncoderBase a }
  deriving newtype Contravariant
  deriving Show via Utilities.ContravariantCoyonedaShow EncoderBase a
