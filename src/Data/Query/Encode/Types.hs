{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Encode.Types where

import           Control.Arrow (Arrow ((&&&)))
import           Data.Fix (Fix (Fix, unFix))
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Coyoneda (Coyoneda (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Prettyprinter as Pretty

-- | Encoder for an item of an enum
newtype ItemEncoder a = ItemEncoder
  { itemEncoder_name :: Text }
  deriving Show

-- | Encoder for a constructor of a variant
data ConstructorEncoder f a = ConstructorEncoder
  { constructorEncoder_name :: Text
  , constructorEncoder_value :: Encoder (f a)
  }
  deriving Show

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

deriving instance Show (FieldSelector a)

-- | Encoder for a field of a record
newtype FieldEncoder a = FieldEncoder
  { unFieldEncoder :: Coyoneda FieldSelector a }
  deriving newtype Contravariant
  deriving Show via Utilities.ContravariantCoyonedaShow FieldSelector a

fieldEncoderToFieldShape :: FieldEncoder a -> Shape.FieldShapeF Shape.Shape
fieldEncoderToFieldShape (FieldEncoder (Coyoneda _ selector)) =
  case selector of
    MandatoryFieldSelector encoder -> Shape.FieldShape
      { Shape.fieldShape_schema = encoderToShape encoder
      , Shape.fieldShape_optional = False
      }

    OptionalFieldSelector encoder -> Shape.FieldShape
      { Shape.fieldShape_schema = encoderToShape encoder
      , Shape.fieldShape_optional = True
      }

data EncoderBase a where
  PrimitiveEncoder
    :: Primitives.Primitive a
    -> EncoderBase a

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
  show = show . Pretty.pretty

instance Pretty.Pretty (EncoderBase a) where
  pretty = Shape.prettyShape . encoderBaseToShape

encoderBaseToShape :: EncoderBase a -> Shape.Shape
encoderBaseToShape = Fix . \case
  PrimitiveEncoder prim -> Shape.Primitive $ Primitives.SomePrimitive prim
  NullableEncoder encoder -> Shape.Nullable $ unFix $ encoderToShape encoder
  ArrayEncoder encoder -> Shape.Array $ encoderToShape encoder
  StringMapEncoder encoder -> Shape.StringMap $ encoderToShape encoder
  EnumEncoder items -> Shape.Enum $ SOP.hcollapse $ SOP.hmap (SOP.K . itemEncoder_name) items
  VariantEncoder constructors ->
    Shape.Variant
    $ HashMap.fromList
    $ SOP.hcollapse
    $ SOP.hmap
        (SOP.K . (constructorEncoder_name &&& encoderToShape . constructorEncoder_value))
        constructors
  RecordEncoder fields ->
    Shape.Record $ HashMap.map fieldEncoderToFieldShape fields

-- | Schematic to encode @a@
newtype Encoder a = Encoder
  { unEncoder :: Coyoneda EncoderBase a }
  deriving newtype Contravariant
  deriving (Show, Pretty.Pretty) via Utilities.ContravariantCoyonedaShow EncoderBase a

encoderToShape :: Encoder a -> Shape.Shape
encoderToShape (Encoder (Coyoneda _ base)) = encoderBaseToShape base
