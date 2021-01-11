{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Query.Decode.Types where

import           Control.Applicative.Free (Ap, runAp_)
import           Data.Fix (Fix (Fix), foldFix)
import           Data.Functor.Coyoneda (Coyoneda (..))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.Query.Value as Value
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Prettyprinter as Pretty
import qualified Type.Reflection as Reflection

-- | Query for @a@
data Query a = Query
  { query_type :: Reflection.TypeRep a
  , query_schema :: Maybe (Decoder a)
  }
  deriving Show

instance Pretty.Pretty (Query a) where
  pretty = Shape.prettyQueryShape . queryToShape

queryToShape :: Query a -> Shape.QueryShape
queryToShape query = Fix Shape.QueryShape
  { Shape.queryShape_type = Reflection.SomeTypeRep $ query_type query
  , Shape.queryShape_shape = decoderToShape <$> query_schema query
  }

-- | Query for a variant constructor
newtype ConstructorQuery a = ConstructorQuery
  { unConstructorQuery :: Coyoneda Query a }
  deriving newtype Functor
  deriving (Show, Pretty.Pretty) via Utilities.CoyonedaShow Query a

constructorQueryToQueryShape :: ConstructorQuery a -> Shape.QueryShape
constructorQueryToQueryShape (ConstructorQuery (Coyoneda _ query)) = queryToShape query

data FieldQuery a where
  MandatoryFieldQuery
    :: Text
    -> Query a
    -> FieldQuery a

  OptionalFieldQuery
    :: Text
    -> Query a
    -> FieldQuery (Maybe a)

deriving instance Show (FieldQuery a)

fieldQueryToFieldShape :: FieldQuery a -> HashMap.HashMap Text (Shape.FieldShapeF Shape.QueryShape)
fieldQueryToFieldShape = \case
  MandatoryFieldQuery name query -> HashMap.singleton name Shape.FieldShapeF
    { Shape.fieldShape_schema = queryToShape query
    , Shape.fieldShape_optional = False
    }

  OptionalFieldQuery name query -> HashMap.singleton name Shape.FieldShapeF
    { Shape.fieldShape_schema = queryToShape query
    , Shape.fieldShape_optional = True
    }

newtype FieldsDecoder a = FieldsDecoder
  { unFieldDecoder :: Ap FieldQuery a }
  deriving newtype (Functor, Applicative)

instance Show (FieldsDecoder a) where
  show = show . Pretty.pretty

instance Pretty.Pretty (FieldsDecoder a) where
  pretty =
    Utilities.runPrettyM
    . Shape.prettyShapeF
    . fmap (foldFix Shape.prettyQueryShapeF)
    . Shape.Record . fieldDecoderToFieldShapes

fieldDecoderToFieldShapes :: FieldsDecoder b -> HashMap.HashMap Text (Shape.FieldShapeF Shape.QueryShape)
fieldDecoderToFieldShapes = runAp_ fieldQueryToFieldShape . unFieldDecoder

data DecoderBase a where
  BoolDecoder
    :: DecoderBase Bool

  NumberDecoder
    :: DecoderBase Scientific

  StringDecoder
    :: DecoderBase Text

  NullableDecoder
    :: DecoderBase a
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
  show = show . Pretty.pretty

instance Pretty.Pretty (DecoderBase a) where
  pretty =
    Utilities.runPrettyM
    . Shape.prettyShapeF
    . fmap (foldFix Shape.prettyQueryShapeF)
    . decoderBaseToShape

decoderBaseToShape :: DecoderBase a -> Shape.ShapeF Shape.QueryShape
decoderBaseToShape = \case
  BoolDecoder -> Shape.Bool
  NumberDecoder -> Shape.Number
  StringDecoder -> Shape.String
  NullableDecoder base -> Shape.Nullable $ decoderBaseToShape base
  ArrayDecoder decoder -> Shape.Array $ queryToShape decoder
  StringMapDecoder decoder -> Shape.StringMap $ queryToShape decoder
  EnumDecoder items -> Shape.Enum $ HashMap.keys items
  VariantDecoder constructors -> Shape.Variant $ constructorQueryToQueryShape <$> constructors
  RecordDecoder fields -> Shape.Record $ fieldDecoderToFieldShapes fields

newtype Decoder a = Decoder
  { unDecoder :: Coyoneda DecoderBase a }
  deriving newtype Functor
  deriving (Show, Pretty.Pretty) via Utilities.CoyonedaShow DecoderBase a

decoderToShape :: Decoder a -> Shape.ShapeF Shape.QueryShape
decoderToShape (Decoder (Coyoneda _ base)) = decoderBaseToShape base

data ConstructorMatch a = ConstructorMatch
  { constructorMatch_name :: Text
  , constructorMatch_query :: ConstructorQuery a
  , constructorMatch_value :: Value.Value
  }
  deriving Show

data DecodeError
  = forall a. UnexpectedInput (DecoderBase a) Value.NoCallValue
  | forall a. UndecodableNeedsCall (Reflection.TypeRep a) Value.NoCallValue
  | MissingField Text
  | UnknownEnum (HashSet Text) Text
  | NoMatchingConstructor (HashSet Text) Value.Object
  | forall a. MultipleConstructors [ConstructorMatch a]

deriving instance Show DecodeError
