{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

  , float
  , double
  , number

  , int8
  , int16
  , int32
  , int64
  , int
  , integer

  , word8
  , word16
  , word32
  , word64
  , word
  , natural

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
  , enum'
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
  , optionalFieldDefault
  , optionalFieldWith
  , optionalFieldDefaultWith

    -- * Shapes
  , Types.querySchemaToQueryShape
  , Types.schemaToShape
  , Types.fieldsSchemaToFieldShapes
  )
where

import           Control.Applicative.Free (liftAp)
import           Data.Coerce (coerce)
import           Data.Fix (Fix (Fix), unFix)
import           Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import           Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict as IntMap
import           Data.Kind (Type)
import           Data.Maybe (fromMaybe)
import           Data.Profunctor (Profunctor (dimap), lmap)
import           Data.Profunctor.Yoneda (Coyoneda (Coyoneda), returnCoyoneda)
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Primitives as Primitives
import qualified Data.Query.Schema.Types as Types
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Data.Word (Word16, Word32, Word64, Word8)
import           GHC.Generics (Generic)
import qualified Generics.SOP
import           Numeric.Natural (Natural)
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

instance HasSchema Float where
  schema = float

instance HasSchema Double where
  schema = double

instance HasSchema Scientific where
  schema = number

instance HasSchema Int8 where
  schema = int8

instance HasSchema Int16 where
  schema = int16

instance HasSchema Int32 where
  schema = int32

instance HasSchema Int64 where
  schema = int64

instance HasSchema Int where
  schema = int

instance HasSchema Integer where
  schema = integer

instance HasSchema Word8 where
  schema = word8

instance HasSchema Word16 where
  schema = word16

instance HasSchema Word32 where
  schema = word32

instance HasSchema Word64 where
  schema = word64

instance HasSchema Word where
  schema = word

instance HasSchema Natural where
  schema = natural

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

instance HasSchema a => HasSchema (Primitives.Limit a)

instance HasSchema a => HasSchema (Primitives.NumberInfo a) where
  schema = recordWith $
    Primitives.NumberInfo
      <$> optionalFieldDefault "lowerLimit" Primitives.NoLimit (Just . Primitives.numberInfo_lowerLimit)
      <*> optionalFieldDefault "upperLimit" Primitives.NoLimit (Just . Primitives.numberInfo_upperLimit)

instance HasSchema a => HasSchema (Primitives.IntegerInfo a) where
  schema = recordWith $
    Primitives.IntegerInfo
      <$> optionalFieldDefault "lowerLimit" Primitives.NoLimit (Just . Primitives.integerInfo_lowerLimit)
      <*> optionalFieldDefault "upperLimit" Primitives.NoLimit (Just . Primitives.integerInfo_upperLimit)
      <*> optionalField "multipleOf" Primitives.integerInfo_multipleOf

data PrimNumberFormat
  = PrimNoNumberFormat (Primitives.NumberInfo Scientific)
  | PrimFloatFormat (Primitives.NumberInfo Float)
  | PrimDoubleFormat (Primitives.NumberInfo Double)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
  deriving HasSchema via
    Generic.CustomGeneric '[Generic.TrimVariantConstructorPrefix "Prim"] PrimNumberFormat

data PrimIntegerFormat
  = PrimNoIntegerFormat (Primitives.IntegerInfo Integer)
  | PrimInt32Format (Primitives.IntegerInfo Int32)
  | PrimInt64Format (Primitives.IntegerInfo Int64)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
  deriving HasSchema via
    Generic.CustomGeneric '[Generic.TrimVariantConstructorPrefix "Prim"] PrimIntegerFormat

data PrimStringFormat
  = PrimNoStringFormat
  | PrimByteFormat
  | PrimBinaryFormat
  | PrimDateFormat
  | PrimDateTimeFormat
  | PrimPasswordFormat
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
  deriving HasSchema via
    Generic.CustomGeneric '[Generic.TrimEnumItemPrefix "Prim"] PrimStringFormat

data Prim
  = PrimBool
  | PrimNumber PrimNumberFormat
  | PrimInteger PrimIntegerFormat
  | PrimString PrimStringFormat
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
  deriving HasSchema via
    Generic.CustomGeneric '[Generic.TrimVariantConstructorPrefix "Prim"] Prim

instance HasSchema Primitives.SomePrimitive where
  schema =
    dimap toPrim fromPrim schema
    where
      toPrim :: Primitives.SomePrimitive -> Prim
      toPrim (Primitives.SomePrimitive prim) =
        case prim of
          Primitives.Boolean -> PrimBool

          Primitives.Number format info -> PrimNumber $ info &
            case format of
              Primitives.NoNumberFormat -> PrimNoNumberFormat
              Primitives.FloatFormat -> PrimFloatFormat
              Primitives.DoubleFormat  -> PrimDoubleFormat

          Primitives.Integer format info -> PrimInteger $ info &
            case format of
              Primitives.NoIntegerFormat -> PrimNoIntegerFormat
              Primitives.Int32Format -> PrimInt32Format
              Primitives.Int64Format -> PrimInt64Format

          Primitives.String format -> PrimString $
            case format of
              Primitives.NoStringFormat -> PrimNoStringFormat
              Primitives.ByteFormat -> PrimByteFormat
              Primitives.BinaryFormat -> PrimBinaryFormat
              Primitives.DateFormat -> PrimDateFormat
              Primitives.DateTimeFormat -> PrimDateTimeFormat
              Primitives.PasswordFormat -> PrimPasswordFormat

      fromPrim :: Prim -> Primitives.SomePrimitive
      fromPrim = \case
        PrimBool -> Primitives.SomePrimitive Primitives.Boolean

        PrimNumber format -> let make f = Primitives.SomePrimitive . Primitives.Number f in
          case format of
            PrimNoNumberFormat info -> make Primitives.NoNumberFormat info
            PrimFloatFormat info -> make Primitives.FloatFormat info
            PrimDoubleFormat info -> make Primitives.DoubleFormat info

        PrimInteger format -> let make f = Primitives.SomePrimitive . Primitives.Integer f in
          case format of
            PrimNoIntegerFormat info -> make Primitives.NoIntegerFormat info
            PrimInt32Format info -> make Primitives.Int32Format info
            PrimInt64Format info -> make Primitives.Int64Format info

        PrimString format -> let make = Primitives.SomePrimitive . Primitives.String in
          case format of
            PrimNoStringFormat -> make Primitives.NoStringFormat
            PrimByteFormat -> make Primitives.ByteFormat
            PrimBinaryFormat -> make Primitives.BinaryFormat
            PrimDateFormat -> make Primitives.DateFormat
            PrimDateTimeFormat -> make Primitives.DateTimeFormat
            PrimPasswordFormat -> make Primitives.PasswordFormat

instance HasSchema a => HasSchema (Shape.FieldShapeF a) where
  schema = record

instance HasSchema a => HasSchema (Shape.ShapeF a)

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
querySchemaWith =
  Types.QuerySchema Reflection.typeRep . Right

-- * Schemas

liftBase :: Types.SchemaBase a b -> Types.Schema a b
liftBase = Types.Schema . returnCoyoneda

-- | Primitive schema
primitive :: Primitives.Primitive a -> Types.Schema a a
primitive prim = liftBase $ Types.PrimitiveSchema prim

-- | Boolean schema
bool :: Types.Schema Bool Bool
bool = primitive Primitives.Boolean

-- | Float schema
float :: Types.Schema Float Float
float = primitive $ Primitives.Number Primitives.FloatFormat Primitives.NumberInfo
  { Primitives.numberInfo_lowerLimit = Primitives.NoLimit
  , Primitives.numberInfo_upperLimit = Primitives.NoLimit
  }

-- | Double schema
double :: Types.Schema Double Double
double = primitive $ Primitives.Number Primitives.DoubleFormat Primitives.NumberInfo
  { Primitives.numberInfo_lowerLimit = Primitives.NoLimit
  , Primitives.numberInfo_upperLimit = Primitives.NoLimit
  }

-- | Numeric schema
number :: Types.Schema Scientific Scientific
number = primitive $ Primitives.Number Primitives.NoNumberFormat Primitives.NumberInfo
  { Primitives.numberInfo_lowerLimit = Primitives.NoLimit
  , Primitives.numberInfo_upperLimit = Primitives.NoLimit
  }

boundedIntegral
  :: forall a b
  .  (Num a, Integral a, Integral b, Bounded a)
  => Primitives.IntegerFormat b
  -> Types.Schema a a
boundedIntegral format =
  dimap fromIntegral fromIntegral $ primitive $ Primitives.Integer format Primitives.IntegerInfo
    { Primitives.integerInfo_lowerLimit = Primitives.InclusiveLimit $ fromIntegral $ minBound @a
    , Primitives.integerInfo_upperLimit = Primitives.InclusiveLimit $ fromIntegral $ maxBound @a
    , Primitives.integerInfo_multipleOf = Nothing
    }

-- | Int8 schema
int8 :: Types.Schema Int8 Int8
int8 = boundedIntegral Primitives.Int32Format

-- | Int16 schema
int16 :: Types.Schema Int16 Int16
int16 = boundedIntegral Primitives.Int32Format

-- | Int32 schema
int32 :: Types.Schema Int32 Int32
int32 = primitive $ Primitives.Integer Primitives.Int32Format Primitives.IntegerInfo
  { Primitives.integerInfo_lowerLimit = Primitives.NoLimit
  , Primitives.integerInfo_upperLimit = Primitives.NoLimit
  , Primitives.integerInfo_multipleOf = Nothing
  }

-- | Int64 schema
int64 :: Types.Schema Int64 Int64
int64 = primitive $ Primitives.Integer Primitives.Int64Format Primitives.IntegerInfo
  { Primitives.integerInfo_lowerLimit = Primitives.NoLimit
  , Primitives.integerInfo_upperLimit = Primitives.NoLimit
  , Primitives.integerInfo_multipleOf = Nothing
  }

-- | Int schema
int :: Types.Schema Int Int
int = boundedIntegral Primitives.NoIntegerFormat

-- | Integer schema
integer :: Types.Schema Integer Integer
integer = primitive $ Primitives.Integer Primitives.NoIntegerFormat Primitives.IntegerInfo
  { Primitives.integerInfo_lowerLimit = Primitives.NoLimit
  , Primitives.integerInfo_upperLimit = Primitives.NoLimit
  , Primitives.integerInfo_multipleOf = Nothing
  }

-- | Word8 schema
word8 :: Types.Schema Word8 Word8
word8 = boundedIntegral Primitives.NoIntegerFormat

-- | Word16 schema
word16 :: Types.Schema Word16 Word16
word16 = boundedIntegral Primitives.NoIntegerFormat

-- | Word32 schema
word32 :: Types.Schema Word32 Word32
word32 = boundedIntegral Primitives.NoIntegerFormat

-- | Word64 schema
word64 :: Types.Schema Word64 Word64
word64 = boundedIntegral Primitives.NoIntegerFormat

-- | Word schema
word :: Types.Schema Word Word
word = boundedIntegral Primitives.NoIntegerFormat

-- | Natural schema
natural :: Types.Schema Natural Natural
natural = dimap toInteger fromInteger $ primitive $
  Primitives.Integer Primitives.NoIntegerFormat Primitives.IntegerInfo
    { Primitives.integerInfo_lowerLimit = Primitives.InclusiveLimit 0
    , Primitives.integerInfo_upperLimit = Primitives.NoLimit
    , Primitives.integerInfo_multipleOf = Nothing
    }

-- | String schema
string :: Types.Schema Text Text
string = primitive $ Primitives.String Primitives.NoStringFormat

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
--
-- Note: While in practise this schema isn't partial, a bad 'Enum' implementation can make it so.
--
enum :: forall a. (Bounded a, Enum a, Show a) => Types.Schema a a
enum = enum' (Text.pack . show)

-- | Schema for an enum type which allows overloading its string representation
--
-- Note: While in practise this schema isn't partial, a bad 'Enum' implementation can make it so.
--
enum' :: forall a. (Bounded a, Enum a) => (a -> Text) -> Types.Schema a a
enum' show =
  Utilities.instantiateProduct [minBound .. maxBound @a] $ \product ->
    let
      items = SOP.hmap (\(SOP.K x) -> itemWith (show x) SOP.Proxy) product

      valueMap =
        IntMap.fromList $ SOP.hcollapse $
          SOP.hzipWith
            (\(SOP.K value) (SOP.Fn f) ->
              SOP.K (fromEnum value, SOP.unK (f SOP.Proxy))
            )
            product
            SOP.injections

      toNS value =
        fromMaybe (error "Unknown enum value") $
          IntMap.lookup (fromEnum value) valueMap

      fromNS sum = SOP.hcollapse $ SOP.hzipWith const product sum
    in
      dimap toNS fromNS $ enumWith items

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

-- | Optional record field with a default value
optionalFieldDefault
  :: HasSchema b
  => Text -- ^ Field name
  -> b -- ^ Default field value
  -> (a -> Maybe b) -- ^ Field accessor
  -> Types.FieldsSchema a b
optionalFieldDefault name =
  optionalFieldDefaultWith name querySchema

-- | Optional record field schema
optionalFieldWith
  :: Text -- ^ Field name
  -> Types.QuerySchema b' b -- ^ Schema for the field value when it is present
  -> (a -> Maybe b') -- ^ Field accessor; returns @Just@ when the field is available
  -> Types.FieldsSchema a (Maybe b)
optionalFieldWith name querySchema f =
  lmap f $ liftFieldSchema $ Types.OptionalFieldSchema name querySchema

-- | Optional record field with a default value
optionalFieldDefaultWith
  :: Text -- ^ Field name
  -> Types.QuerySchema b' b -- ^ Schema for the field value when it is present
  -> b -- ^ Default field value
  -> (a -> Maybe b') -- ^ Field accessor
  -> Types.FieldsSchema a b
optionalFieldDefaultWith name querySchema def access =
  fromMaybe def <$> optionalFieldWith name querySchema access

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
