{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Query.Decode
  ( Types.Query
  , query
  , queryWith
  , undecodableQuery
  , queryType

  , HasDecoder (..)
  , Types.Decoder
  , generic
  , bool
  , number
  , string
  , nullable
  , nullableWith
  , array
  , arrayWith
  , stringMap
  , stringMapWith
  , enum
  , enumWith
  , variantWith

  , Types.ConstructorQuery
  , constructor
  , constructorWith

  , record
  , recordWith

  , HasFieldsDecoder (..)
  , Types.FieldsDecoder
  , genericFields
  , field
  , fieldWith
  , optionalField
  , optionalFieldWith
  )
where

import           Control.Applicative.Free (liftAp)
import           Data.Coerce (coerce)
import           Data.Fix (Fix (Fix))
import qualified Data.Functor.Coyoneda as Coyoneda
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.Profunctor (Profunctor (..))
import qualified Data.Query.Decode.Types as Types
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import           Data.Scientific (Scientific)
import qualified Data.SOP as SOP
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Type.Reflection as Reflection

-- * Classes

class Reflection.Typeable a => HasDecoder a where
  decoder :: Types.Decoder a

  default decoder :: Generic.GHas HasDecoder a => Types.Decoder a
  decoder = generic Generic.defaultOptions

instance HasDecoder () where
  decoder = record

instance HasDecoder Bool where
  decoder = bool

instance HasDecoder Scientific where
  decoder = number

instance HasDecoder Text where
  decoder = string

instance HasDecoder a => HasDecoder (Vector.Vector a) where
  decoder = array

instance HasDecoder a => HasDecoder [a] where
  decoder = Vector.toList <$> array

instance HasDecoder a => HasDecoder (HashMap.HashMap Text a) where
  decoder = stringMap

instance (Reflection.Typeable f, HasDecoder (f (Fix f))) => HasDecoder (Fix f) where
  decoder = Fix <$> decoder

instance
  ( Reflection.Typeable a
  , Reflection.Typeable k
  , Reflection.Typeable options
  , Generic.GHas HasDecoder a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasDecoder (Generic.CustomGeneric (options :: [k]) a)
  where
    decoder = coerce (generic @a (Generic.demoteOptions @options SOP.Proxy))

instance HasDecoder a => HasDecoder (Shape.FieldShapeF a) where
  decoder = record

instance HasDecoder a => HasDecoder (Shape.ShapeF a)

class HasFieldsDecoder a where
  fieldsDecoder :: Types.FieldsDecoder a

  default fieldsDecoder
    :: Generic.GHasFields HasDecoder a
    => Types.FieldsDecoder a
  fieldsDecoder =
    genericFields Generic.defaultOptions

instance HasFieldsDecoder () where
  fieldsDecoder = pure ()

instance
  ( Reflection.Typeable a
  , Reflection.Typeable k
  , Reflection.Typeable options
  , Generic.GHasFields HasDecoder a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasFieldsDecoder (Generic.CustomGeneric (options :: [k]) a)
  where
    fieldsDecoder = coerce (genericFields @a (Generic.demoteOptions @options SOP.Proxy))

deriving
  via Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (Shape.FieldShapeF a)
  instance HasDecoder a => HasFieldsDecoder (Shape.FieldShapeF a)

-- * Queries

query :: HasDecoder a => Types.Query a
query = queryWith decoder

queryWith :: Reflection.Typeable a => Types.Decoder a -> Types.Query a
queryWith = Types.Query Reflection.typeRep . Just

undecodableQuery :: Reflection.Typeable a => Types.Query a
undecodableQuery = Types.Query Reflection.typeRep Nothing

queryType :: Types.Query a -> Reflection.TypeRep a
queryType = Types.query_type

-- * Decoders

liftBase :: Types.DecoderBase a -> Types.Decoder a
liftBase = Types.Decoder . Coyoneda.liftCoyoneda

bool :: Types.Decoder Bool
bool = liftBase Types.BoolDecoder

number :: Types.Decoder Scientific
number = liftBase Types.NumberDecoder

string :: Types.Decoder Text
string = liftBase Types.StringDecoder

nullable :: HasDecoder a => Types.Decoder (Maybe a)
nullable = nullableWith decoder

nullableWith :: Types.Decoder a -> Types.Decoder (Maybe a)
nullableWith = liftBase . Types.NullableDecoder

array :: HasDecoder a => Types.Decoder (Vector.Vector a)
array = arrayWith query

arrayWith :: Types.Query a -> Types.Decoder (Vector.Vector a)
arrayWith = liftBase . Types.ArrayDecoder

stringMap :: HasDecoder a => Types.Decoder (HashMap.HashMap Text a)
stringMap = stringMapWith query

stringMapWith :: Types.Query a -> Types.Decoder (HashMap.HashMap Text a)
stringMapWith = liftBase . Types.StringMapDecoder

enum :: (Show a, Bounded a, Enum a) => Types.Decoder a
enum =
  enumWith $ HashMap.fromList
    [ (Text.pack (show elem), elem)
    | elem <- [minBound .. maxBound]
    ]

enumWith :: HashMap.HashMap Text a -> Types.Decoder a
enumWith = liftBase . Types.EnumDecoder

variantWith :: HashMap.HashMap Text (Types.ConstructorQuery a) -> Types.Decoder a
variantWith = liftBase . Types.VariantDecoder

record :: HasFieldsDecoder a => Types.Decoder a
record = recordWith fieldsDecoder

recordWith :: Types.FieldsDecoder a -> Types.Decoder a
recordWith = liftBase . Types.RecordDecoder

-- * Constructor decoders

constructor :: HasDecoder b => (b -> a) -> Types.ConstructorQuery a
constructor = constructorWith query

constructorWith :: Types.Query b -> (b -> a) -> Types.ConstructorQuery a
constructorWith query f = Types.ConstructorQuery $ Coyoneda.Coyoneda f query

-- * Field decoders

field :: HasDecoder a => Text -> Types.FieldsDecoder a
field name = fieldWith name query

fieldWith :: Text -> Types.Query a -> Types.FieldsDecoder a
fieldWith name query = Types.FieldsDecoder $ liftAp $ Types.MandatoryFieldQuery name query

optionalField :: HasDecoder a => Text -> Types.FieldsDecoder (Maybe a)
optionalField name = optionalFieldWith name query

optionalFieldWith :: Text -> Types.Query a -> Types.FieldsDecoder (Maybe a)
optionalFieldWith name query = Types.FieldsDecoder $ liftAp $ Types.OptionalFieldQuery name query

-- * Generics

deriving
  via Types.FieldsDecoder
  instance Functor (Generic.FieldsSchema HasDecoder a)

deriving
  via Types.FieldsDecoder
  instance Applicative (Generic.FieldsSchema HasDecoder a)

instance Profunctor (Generic.FieldsSchema HasDecoder) where
  lmap _ (GFieldsDecoder decoder) = GFieldsDecoder decoder

  rmap = fmap

instance Generic.SchemaFlavour HasDecoder where
  newtype QuerySchema HasDecoder a = GQuery
    { unGQuery :: Types.Query a }

  newtype Schema HasDecoder a = GDecoder
    { unGDecoder :: Types.Decoder a }

  data ItemSchema HasDecoder f a = GItemDecoder Text (f a)

  data ConstructorSchema HasDecoder f a = GConstructorDecoder Text (Types.ConstructorQuery (f a))

  newtype FieldsSchema HasDecoder _ b = GFieldsDecoder
    { unGFieldsDecoder :: Types.FieldsDecoder b }

  type SupplementalClass HasDecoder = Reflection.Typeable

  querySchema = GQuery query

  querySchemaWith (GDecoder (decoder :: Types.Decoder (SOP.NP SOP.I xs))) =
    GQuery
    $ Reflection.withTypeable (Utilities.typeReps @Type @xs)
    $ queryWith decoder

  schema = GDecoder decoder

  mapSchema _ f decoder = GDecoder $ f <$> unGDecoder decoder

  enumWith items =
    GDecoder $ enumWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(GItemDecoder name value) (SOP.Fn mkSum) -> SOP.K (name, SOP.unK (mkSum value)))
        items
        SOP.injections

  variantWith constructors =
    GDecoder $ variantWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(GConstructorDecoder name query) (SOP.Fn mkSum) ->
          SOP.K (name, fmap (SOP.unK . mkSum) query))
        constructors
        SOP.injections

  recordWith =
    GDecoder . recordWith . unGFieldsDecoder

  fieldWith name query _access =
    GFieldsDecoder $ fieldWith name $ unGQuery query

  optionalFieldWith name query _access =
    GFieldsDecoder $ optionalFieldWith name $ unGQuery query

  item = GItemDecoder

  constructorWith name _ lift query =
    GConstructorDecoder name $ constructorWith (unGQuery query) lift

generic
  :: Generic.GHas HasDecoder a
  => Generic.Options
  -> Types.Decoder a
generic options =
  unGDecoder $ Generic.gSchema options

genericFields
  :: Generic.GHasFields HasDecoder a
  => Generic.Options
  -> Types.FieldsDecoder a
genericFields options =
  unGFieldsDecoder $ Generic.gFieldsSchema options
