{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

    -- * Primitives
  , bool
  , number
  , string

    -- * Nullables
  , nullable
  , nullableWith

    -- * Arrays
  , array
  , arrayWith

    -- * String mappings
  , stringMap
  , stringMapWith

    -- * Enums
  , enum
  , enumWith

    -- * Variants
  , variantWith

    -- ** Constructors
  , Types.ConstructorQuery
  , constructor
  , constructorWith

    -- * Records
  , record
  , recordWith

    -- ** Fields
  , HasFieldsDecoder (..)
  , Types.FieldsDecoder
  , genericFields
  , field
  , fieldWith
  , optionalField
  , optionalFieldWith

    -- * Schema derivation
  , querySchemaQuery
  , querySchemaQueryWith
  , schemaDecoder
  , schemaDecoderWith
  )
where

import           Control.Applicative.Free (liftAp, runAp)
import           Data.Coerce (coerce)
import           Data.Fix (Fix (Fix))
import qualified Data.Functor.Coyoneda as Coyoneda
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.Profunctor (Profunctor (..))
import qualified Data.Profunctor.Yoneda as Profunctor
import qualified Data.Query.Decode.Types as Types
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema as Schema
import qualified Data.Query.Schema.Types as Schema
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
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

-- | See 'queryWith'.
query :: HasDecoder a => Types.Query a
query = queryWith decoder

-- | Query for @a@ where the @a@ can be decoded or instantiated through a function call
queryWith
  :: Reflection.Typeable a
  => Types.Decoder a -- ^ Decoder for when it is not a function call
  -> Types.Query a
queryWith = Types.Query Reflection.typeRep . Just

-- | Query for @a@ where @a@ can only be instantiated through a function call
undecodableQuery :: Reflection.Typeable a => Types.Query a
undecodableQuery = Types.Query Reflection.typeRep Nothing

-- | 'Reflection.TypeRep' for @a@
queryType :: Types.Query a -> Reflection.TypeRep a
queryType = Types.query_type

-- * Decoders

liftBase :: Types.DecoderBase a -> Types.Decoder a
liftBase = Types.Decoder . Coyoneda.liftCoyoneda

-- | Boolean decoder
bool :: Types.Decoder Bool
bool = liftBase Types.BoolDecoder

-- | Numeric decoder
number :: Types.Decoder Scientific
number = liftBase Types.NumberDecoder

-- | String decoder
string :: Types.Decoder Text
string = liftBase Types.StringDecoder

-- | See 'nullableWith'.
nullable :: HasDecoder a => Types.Decoder (Maybe a)
nullable = nullableWith decoder

-- | Decode @a@ but allow decoding @null@ which results in a @Nothing@
nullableWith :: Types.Decoder a -> Types.Decoder (Maybe a)
nullableWith (Types.Decoder (Coyoneda.Coyoneda f base)) =
  Types.Decoder $ Coyoneda.Coyoneda (fmap f) $ Types.NullableDecoder base

-- | See 'arrayWith'.
array :: HasDecoder a => Types.Decoder (Vector.Vector a)
array = arrayWith query

-- | Decode an array of @a@s.
arrayWith
  :: Types.Query a -- ^ Decoder for each item of the array
  -> Types.Decoder (Vector.Vector a)
arrayWith =
  liftBase . Types.ArrayDecoder

-- | See 'stringMapWith'.
stringMap :: HasDecoder a => Types.Decoder (HashMap.HashMap Text a)
stringMap = stringMapWith query

-- | Decode an object mapping strings to values that can be decoded using given 'Types.Query'.
stringMapWith
  :: Types.Query a -- ^ Decoder for each string-mapped value
  -> Types.Decoder (HashMap.HashMap Text a)
stringMapWith =
  liftBase . Types.StringMapDecoder

-- | Decode an enum.
enum :: (Show a, Bounded a, Enum a) => Types.Decoder a
enum =
  enumWith $ HashMap.fromList
    [ (Text.pack (show elem), elem)
    | elem <- [minBound .. maxBound]
    ]

-- | Decode an enum using the given mapping.
enumWith
  :: HashMap.HashMap Text a -- ^ Mapping from stringified representation to actual value
  -> Types.Decoder a
enumWith =
  liftBase . Types.EnumDecoder

-- | Decode a variant.
variantWith
  :: HashMap.HashMap Text (Types.ConstructorQuery a) -- ^ Mapping from constructor name to decoder
  -> Types.Decoder a
variantWith =
  liftBase . Types.VariantDecoder

-- * Constructor decoders

-- | See 'constructorWith'.
constructor :: HasDecoder b => (b -> a) -> Types.ConstructorQuery a
constructor = constructorWith query

-- | Decode a constructor of @a@ via @b@.
constructorWith
  :: Types.Query b -- ^ Decoder for the constructor body
  -> (b -> a) -- ^ Constructor of the variant
  -> Types.ConstructorQuery a
constructorWith query f =
  Types.ConstructorQuery $ Coyoneda.Coyoneda f query

-- | See 'recordWith'.
record
  :: HasFieldsDecoder a
  => Types.Decoder a
record =
  recordWith fieldsDecoder

-- | Decode a record.
recordWith
  :: Types.FieldsDecoder a -- ^ Describes how the fields are decoded to assemble @a@
  -> Types.Decoder a
recordWith =
  liftBase . Types.RecordDecoder

-- * Field decoders

-- | See 'fieldWith'.
field
  :: HasDecoder a
  => Text -- ^ Field name
  -> Types.FieldsDecoder a
field name =
  fieldWith name query

-- | Decode a mandatory field.
fieldWith
  :: Text -- ^ Field name
  -> Types.Query a -- ^ Decoder for the field value
  -> Types.FieldsDecoder a
fieldWith name query =
  Types.FieldsDecoder $ liftAp $ Types.MandatoryFieldQuery name query

-- | See 'optionalFieldWith'.
optionalField
  :: HasDecoder a
  => Text -- ^ Field name
  -> Types.FieldsDecoder (Maybe a)
optionalField name =
  optionalFieldWith name query

-- | Decode an optional field.
optionalFieldWith
  :: Text -- ^ Field name
  -> Types.Query a -- ^ Field value decoder if it is present
  -> Types.FieldsDecoder (Maybe a)
optionalFieldWith name query =
  Types.FieldsDecoder $ liftAp $ Types.OptionalFieldQuery name query

-- * Schema derivation

-- | See 'querySchemaQueryWith'.
querySchemaQuery :: Schema.HasSchema a => Types.Query a
querySchemaQuery = querySchemaQueryWith Schema.querySchema

-- | Convert a 'Schema.QuerySchema' to a 'Types.Query'.
querySchemaQueryWith :: Schema.QuerySchema a b -> Types.Query b
querySchemaQueryWith querySchema =
  Reflection.withTypeable (Schema.querySchema_type querySchema) $
    case Schema.querySchema_schema querySchema of
      Right schema -> queryWith $ schemaDecoderWith schema
      Left _ -> undecodableQuery

-- | See 'schemaDecoderWith'.
schemaDecoder :: Schema.HasSchema a => Types.Decoder a
schemaDecoder = schemaDecoderWith Schema.schema

-- | Convert a 'Schema.Schema' to a 'Types.Decoder'.
schemaDecoderWith :: Schema.Schema a b -> Types.Decoder b
schemaDecoderWith (Schema.Schema (Profunctor.Coyoneda _ f schemaBase)) =
  f <$> schemaBaseDecoderWith schemaBase

schemaBaseDecoderWith :: Schema.SchemaBase a b -> Types.Decoder b
schemaBaseDecoderWith = \case
  Schema.BoolSchema ->
    bool

  Schema.NumberSchema ->
    number

  Schema.StringSchema ->
    string

  Schema.NullableSchema schema ->
    nullableWith $ schemaDecoderWith schema

  Schema.ArraySchema schema ->
    arrayWith $ querySchemaQueryWith schema

  Schema.StringMapSchema schema ->
    stringMapWith $ querySchemaQueryWith schema

  Schema.EnumSchema items ->
    enumWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(Schema.ItemSchema name value) (SOP.Fn mkEnum) -> SOP.K (name, SOP.unK (mkEnum value)))
        items
        SOP.injections

  Schema.VariantSchema constructors ->
    variantWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(Schema.ConstructorSchema name (Profunctor.Coyoneda _ lift schema)) (SOP.Fn construct) -> do
          let query = querySchemaQueryWith schema
          let mkValue = SOP.unK . construct . lift
          SOP.K (name, constructorWith query mkValue)
        )
        constructors
        SOP.injections

  Schema.RecordSchema fields ->
    recordWith $
      runAp
        (\(Profunctor.Coyoneda _ g fieldSchema) ->
          case fieldSchema of
            Schema.MandatoryFieldSchema name schema ->
              fmap g $ fieldWith name $ querySchemaQueryWith schema

            Schema.OptionalFieldSchema name schema ->
              fmap g $ optionalFieldWith name $ querySchemaQueryWith schema
        )
        (Schema.unFieldsSchema fields)

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

-- | Generic decoder for @a@ if it implements instances for SOP generics
generic
  :: Generic.GHas HasDecoder a
  => Generic.Options
  -> Types.Decoder a
generic options =
  unGDecoder $ Generic.gSchema options

-- | Generic fields decoder for @a@ if it implements instances for SOP generics
genericFields
  :: Generic.GHasFields HasDecoder a
  => Generic.Options
  -> Types.FieldsDecoder a
genericFields options =
  unGFieldsDecoder $ Generic.gFieldsSchema options
