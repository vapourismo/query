{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Query.Schema
  ( Types.QuerySchema
  , querySchema
  , querySchemaWith

  , HasSchema (..)
  , Types.Schema
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
  , enumWith

    -- ** Items
  , Types.ItemSchema
  , itemWith

    -- * Variants
  , variantWith

    -- ** Constructors
  , Types.ConstructorSchema
  , dimapConstructor
  , constructor
  , constructorWith

    -- * Records
  , record
  , recordWith

    -- * Fields
  , HasFieldsSchema (..)
  , Types.FieldsSchema
  , genericFields

  , field
  , fieldWith
  , optionalField
  , optionalFieldWith
  )
where

import           Control.Applicative.Free (liftAp)
import           Data.Coerce (coerce)
import           Data.Fix (Fix (Fix), unFix)
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.Profunctor (Profunctor (dimap), lmap)
import           Data.Profunctor.Yoneda (Coyoneda (Coyoneda), returnCoyoneda)
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema.Types as Types
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Type.Reflection as Reflection

-- * Classes

class Reflection.Typeable a => HasSchema a where
  schema :: Types.Schema a a

  default schema :: Generic.GHas HasSchema a => Types.Schema a a
  schema = generic Generic.defaultOptions

instance HasSchema () where
  schema = record

instance HasSchema Bool where
  schema = bool

instance HasSchema Scientific where
  schema = number

instance HasSchema Text where
  schema = string

instance HasSchema a => HasSchema (Vector.Vector a) where
  schema = array

instance HasSchema a => HasSchema [a] where
  schema = dimap Vector.fromList Vector.toList array

instance HasSchema a => HasSchema (HashMap.HashMap Text a) where
  schema = stringMap

instance (Reflection.Typeable f, HasSchema (f (Fix f))) => HasSchema (Fix f) where
  schema = dimap unFix Fix schema

instance
  ( Reflection.Typeable a
  , Reflection.Typeable k
  , Reflection.Typeable options
  , Generic.GHas HasSchema a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasSchema (Generic.CustomGeneric (options :: [k]) a)
  where
    schema = coerce (generic @a (Generic.demoteOptions @options SOP.Proxy))

instance HasSchema a => HasSchema (Shape.FieldShapeF a) where
  schema = record

instance HasSchema a => HasSchema (Shape.ShapeF a)

class HasFieldsSchema a where
  fieldsSchema :: Types.FieldsSchema a a

  default fieldsSchema :: Generic.GHasFields HasSchema a => Types.FieldsSchema a a
  fieldsSchema = genericFields Generic.defaultOptions

instance HasFieldsSchema () where
  fieldsSchema = pure ()

instance
  ( Reflection.Typeable a
  , Reflection.Typeable k
  , Reflection.Typeable options
  , Generic.GHasFields HasSchema a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasFieldsSchema (Generic.CustomGeneric (options :: [k]) a)
  where
    fieldsSchema = coerce (genericFields @a (Generic.demoteOptions @options SOP.Proxy))

deriving
  via Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (Shape.FieldShapeF a)
  instance HasSchema a => HasFieldsSchema (Shape.FieldShapeF a)

-- * Query schemas

-- See 'querySchemaWith'.
querySchema :: HasSchema a => Types.QuerySchema a a
querySchema = querySchemaWith schema

-- | Schema for an encodable type @a@ and a decodable type @b@ that also be instantiated through a
-- function call.
querySchemaWith
  :: Reflection.Typeable b
  => Types.Schema a b
  -> Types.QuerySchema a b
querySchemaWith = Types.QuerySchema Reflection.typeRep . Right

-- * Schemas

liftBase :: Types.SchemaBase a b -> Types.Schema a b
liftBase = Types.Schema . returnCoyoneda

-- | Boolean schema
bool :: Types.Schema Bool Bool
bool = liftBase Types.BoolSchema

-- | Numeric schema
number :: Types.Schema Scientific Scientific
number = liftBase Types.NumberSchema

-- | String schema
string :: Types.Schema Text Text
string = liftBase Types.StringSchema

-- | See 'nullableWith'.
nullable :: HasSchema a => Types.Schema (Maybe a) (Maybe a)
nullable = nullableWith schema

-- | Schema for @a@ and @b@
nullableWith :: Types.Schema a b -> Types.Schema (Maybe a) (Maybe b)
nullableWith = liftBase . Types.NullableSchema

-- | See 'arrayWith'.
array :: HasSchema a => Types.Schema (Vector.Vector a) (Vector.Vector a)
array = arrayWith querySchema

-- | Schema for an array that is encoded with items of @a@ and decoded to items of @b@.
arrayWith
  :: Types.QuerySchema a b -- ^ Item schema
  -> Types.Schema (Vector.Vector a) (Vector.Vector b)
arrayWith =
  liftBase . Types.ArraySchema

-- | See 'stringMapWith'.
stringMap
  :: HasSchema a
  => Types.Schema (HashMap.HashMap Text a) (HashMap.HashMap Text a)
stringMap =
  stringMapWith querySchema

-- | Schema for an object that maps strings to values
stringMapWith
  :: Types.QuerySchema a b -- ^ String-mapped value schema
  -> Types.Schema (HashMap.HashMap Text a) (HashMap.HashMap Text b)
stringMapWith =
  liftBase . Types.StringMapSchema

-- | Schema for an enum
enumWith
  :: SOP.SListI xs
  => SOP.NP (Types.ItemSchema f) xs
  -> Types.Schema (SOP.NS f xs) (SOP.NS f xs)
enumWith =
  liftBase . Types.EnumSchema

-- | Schema for an item of an enum
itemWith
  :: Text -- ^ Item identifier
  -> f a -- ^ Enum value
  -> Types.ItemSchema f a
itemWith =
  Types.ItemSchema

-- | Variant schema
variantWith
  :: SOP.SListI xs
  => SOP.NP (Types.ConstructorSchema f) xs -- ^ Constructors
  -> Types.Schema (SOP.NS f xs) (SOP.NS f xs)
variantWith =
  liftBase . Types.VariantSchema

-- | Transform a 'Types.ConstructorSchema'.
dimapConstructor
  :: (g b -> f a)
  -> (f a -> g b)
  -> Types.ConstructorSchema f a
  -> Types.ConstructorSchema g b
dimapConstructor f g schema = Types.ConstructorSchema
  { Types.constructorSchema_name = Types.constructorSchema_name schema
  , Types.constructorSchema_schema = dimap f g $ Types.constructorSchema_schema schema
  }

-- | See 'constructorWith'.
constructor
  :: HasSchema a
  => Text -- ^ Constructor name
  -> Types.ConstructorSchema SOP.I a
constructor name =
  constructorWith name SOP.unI SOP.I querySchema

-- | Constructor schema
constructorWith
  :: Text -- ^ Constructor name
  -> (f a -> b) -- ^ Contravariant
  -> (c -> f a) -- ^ Covariant
  -> Types.QuerySchema b c -- ^ Queryable schema that will be used to get the constructor value
  -> Types.ConstructorSchema f a
constructorWith name f g query =
  Types.ConstructorSchema name $ Coyoneda f g query

-- | See 'recordWith'.
record :: HasFieldsSchema a => Types.Schema a a
record = recordWith fieldsSchema

-- | Record schema
recordWith
  :: Types.FieldsSchema a b -- ^ Schema describing the fields of the record
  -> Types.Schema a b
recordWith =
  liftBase . Types.RecordSchema

liftFieldSchema :: Types.FieldSchema a b -> Types.FieldsSchema a b
liftFieldSchema = Types.FieldsSchema . liftAp . returnCoyoneda

-- | See 'fieldWith'.
field
  :: HasSchema b
  => Text -- ^ Field name
  -> (a -> b) -- ^ Field accessor
  -> Types.FieldsSchema a b
field name =
  fieldWith name querySchema

-- | Mandatory record field schema
fieldWith
  :: Text -- ^ Field name
  -> Types.QuerySchema b' b -- ^ Schema for the field value
  -> (a -> b') -- ^ Accessor
  -> Types.FieldsSchema a b
fieldWith name querySchema f =
  lmap f $ liftFieldSchema $ Types.MandatoryFieldSchema name querySchema

-- | See 'optionalFieldWith'.
optionalField
  :: HasSchema b
  => Text -- ^ Field name
  -> (a -> Maybe b) -- ^ Field accessor
  -> Types.FieldsSchema a (Maybe b)
optionalField name =
  optionalFieldWith name querySchema

-- | Optional record field schema
optionalFieldWith
  :: Text -- ^ Field name
  -> Types.QuerySchema b' b -- ^ Schema for the field value when it is present
  -> (a -> Maybe b') -- ^ Field accessor; returns @Just@ when the field is available
  -> Types.FieldsSchema a (Maybe b)
optionalFieldWith name querySchema f =
  lmap f $ liftFieldSchema $ Types.OptionalFieldSchema name querySchema

-- * Generics

deriving
  via Types.FieldsSchema a
  instance Functor (Generic.FieldsSchema HasSchema a)

deriving
  via Types.FieldsSchema a
  instance Applicative (Generic.FieldsSchema HasSchema a)

deriving
  via Types.FieldsSchema
  instance Profunctor (Generic.FieldsSchema HasSchema)

instance Generic.SchemaFlavour HasSchema where
  newtype QuerySchema HasSchema a = GQuerySchema
    { unGQuerySchema :: Types.QuerySchema a a }

  newtype Schema HasSchema a = GSchema
    { unGSchema :: Types.Schema a a }

  newtype ItemSchema HasSchema f a = GItemSchema
    { unGItemSchema :: Types.ItemSchema f a }

  newtype ConstructorSchema HasSchema f a = GConstructorSchema
    { unGConstructorSchema :: Types.ConstructorSchema f a }

  newtype FieldsSchema HasSchema a b = GFieldsSchema
    { unGFieldsSchema :: Types.FieldsSchema a b }

  type SupplementalClass HasSchema = Reflection.Typeable

  querySchema = GQuerySchema querySchema

  querySchemaWith (GSchema (schema :: Types.Schema (SOP.NP SOP.I xs) (SOP.NP SOP.I xs))) =
    GQuerySchema
    $ Reflection.withTypeable (Utilities.typeReps @Type @xs)
    $ querySchemaWith schema

  schema = GSchema schema

  mapSchema f g schema = GSchema $ dimap f g $ unGSchema schema

  enumWith = GSchema . enumWith . SOP.hmap unGItemSchema

  variantWith = GSchema . variantWith . SOP.hmap unGConstructorSchema

  recordWith = GSchema . recordWith . unGFieldsSchema

  fieldWith name schema f = GFieldsSchema $ fieldWith name (unGQuerySchema schema) f

  optionalFieldWith name schema f = GFieldsSchema $ optionalFieldWith name (unGQuerySchema schema) f

  item name a = GItemSchema $ itemWith name a

  constructorWith name extract lift (GQuerySchema querySchema) =
    GConstructorSchema $ constructorWith name extract lift querySchema

generic
  :: Generic.GHas HasSchema a
  => Generic.Options
  -> Types.Schema a a
generic options =
  unGSchema $ Generic.gSchema options

genericFields
  :: Generic.GHasFields HasSchema a
  => Generic.Options
  -> Types.FieldsSchema a a
genericFields options =
  unGFieldsSchema $ Generic.gFieldsSchema options
