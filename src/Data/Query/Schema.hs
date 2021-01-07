{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  , bool
  , number
  , string
  , nullable
  , nullableWith
  , array
  , arrayWith
  , stringMap
  , stringMapWith
  , enumWith

  , Types.ItemSchema
  , itemWith

  , variantWith

  , Types.ConstructorSchema
  , constructor
  , constructorWith

  , record
  , recordWith

  , HasFieldsSchema (..)
  , Types.FieldsSchema
  , genericFields
  , field
  , fieldWith
  , optionalField
  , optionalFieldWith

  , querySchemaToEncoder
  , schemaToEncoder
  , querySchemaToQuery
  , schemaToDecoder
  )
where

import           Control.Applicative.Free (liftAp, runAp, runAp_)
import           Data.Coerce (coerce)
import           Data.Fix (Fix (Fix), unFix)
import           Data.Functor.Contravariant (Contravariant (contramap))
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.Profunctor (Profunctor (dimap), lmap)
import           Data.Profunctor.Yoneda (Coyoneda (Coyoneda), returnCoyoneda)
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Encode as Encode
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema.Types as Types
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Utilities as Utilities
import           Data.Scientific (Scientific)
import qualified Data.SOP as SOP
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

querySchema :: HasSchema a => Types.QuerySchema a a
querySchema = querySchemaWith schema

querySchemaWith :: Reflection.Typeable b => Types.Schema a b -> Types.QuerySchema a b
querySchemaWith = Types.QuerySchema Reflection.typeRep . Right

-- * Schemas

liftBase :: Types.SchemaBase a b -> Types.Schema a b
liftBase = Types.Schema . returnCoyoneda

bool :: Types.Schema Bool Bool
bool = liftBase Types.BoolSchema

number :: Types.Schema Scientific Scientific
number = liftBase Types.NumberSchema

string :: Types.Schema Text Text
string = liftBase Types.StringSchema

nullable :: HasSchema a => Types.Schema (Maybe a) (Maybe a)
nullable = nullableWith schema

nullableWith :: Types.Schema a b -> Types.Schema (Maybe a) (Maybe b)
nullableWith = liftBase . Types.NullableSchema

array :: HasSchema a => Types.Schema (Vector.Vector a) (Vector.Vector a)
array = arrayWith querySchema

arrayWith :: Types.QuerySchema a b -> Types.Schema (Vector.Vector a) (Vector.Vector b)
arrayWith = liftBase . Types.ArraySchema

stringMap
  :: HasSchema a
  => Types.Schema (HashMap.HashMap Text a) (HashMap.HashMap Text a)
stringMap =
  stringMapWith querySchema

stringMapWith
  :: Types.QuerySchema a b
  -> Types.Schema (HashMap.HashMap Text a) (HashMap.HashMap Text b)
stringMapWith =
  liftBase . Types.StringMapSchema

enumWith
  :: SOP.SListI xs
  => SOP.NP (Types.ItemSchema f) xs
  -> Types.Schema (SOP.NS f xs) (SOP.NS f xs)
enumWith =
  liftBase . Types.EnumSchema

itemWith :: Text -> f a -> Types.ItemSchema f a
itemWith = Types.ItemSchema

variantWith
  :: SOP.SListI xs
  => SOP.NP (Types.ConstructorSchema f) xs
  -> Types.Schema (SOP.NS f xs) (SOP.NS f xs)
variantWith =
  liftBase . Types.VariantSchema

constructor
  :: HasSchema a
  => Text
  -> Types.ConstructorSchema SOP.I a
constructor name =
  constructorWith name SOP.unI SOP.I querySchema

constructorWith
  :: Text
  -> (f a -> b)
  -> (c -> f a)
  -> Types.QuerySchema b c
  -> Types.ConstructorSchema f a
constructorWith name f g query =
  Types.ConstructorSchema name $ Coyoneda f g query

record :: HasFieldsSchema a => Types.Schema a a
record = recordWith fieldsSchema

recordWith :: Types.FieldsSchema a b -> Types.Schema a b
recordWith = liftBase . Types.RecordSchema

liftFieldSchema :: Types.FieldSchema a b -> Types.FieldsSchema a b
liftFieldSchema = Types.FieldsSchema . liftAp . returnCoyoneda

field :: HasSchema b => Text -> (a -> b) -> Types.FieldsSchema a b
field name = fieldWith name querySchema

fieldWith
  :: Text
  -> Types.QuerySchema a' b
  -> (a -> a')
  -> Types.FieldsSchema a b
fieldWith name querySchema f =
  lmap f $ liftFieldSchema $ Types.MandatoryFieldSchema name querySchema

optionalField
  :: HasSchema b
  => Text
  -> (a -> Maybe b)
  -> Types.FieldsSchema a (Maybe b)
optionalField name =
  optionalFieldWith name querySchema

optionalFieldWith
  :: Text
  -> Types.QuerySchema a' b
  -> (a -> Maybe a')
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

-- * Shapes

-- * Conversion

querySchemaToEncoder :: Types.QuerySchema a b -> Encode.Encoder a
querySchemaToEncoder querySchema =
  case Types.querySchema_schema querySchema of
    Left encoder -> encoder
    Right schema -> schemaToEncoder schema

schemaToEncoder :: Types.Schema a b -> Encode.Encoder a
schemaToEncoder (Types.Schema (Coyoneda f _ schemaBase)) =
  contramap f $ schemaBaseToEncoder schemaBase

schemaBaseToEncoder :: Types.SchemaBase a b -> Encode.Encoder a
schemaBaseToEncoder = \case
  Types.BoolSchema ->
    Encode.bool

  Types.NumberSchema ->
    Encode.number

  Types.StringSchema ->
    Encode.string

  Types.NullableSchema schema ->
    Encode.nullableWith $ schemaToEncoder schema

  Types.ArraySchema schema ->
    Encode.arrayWith $ querySchemaToEncoder schema

  Types.StringMapSchema schema ->
    Encode.stringMapWith $ querySchemaToEncoder schema

  Types.EnumSchema items ->
    Encode.enumWith $
      SOP.hmap
        (\(Types.ItemSchema name _) -> Encode.item name)
        items

  Types.VariantSchema constructors ->
    Encode.variantWith $
      SOP.hmap
        (\(Types.ConstructorSchema name (Coyoneda extract _ schema)) ->
          Encode.constructorWith name (contramap extract (querySchemaToEncoder schema))
        )
        constructors

  Types.RecordSchema (Types.FieldsSchema fields) ->
    Encode.recordWith $
      runAp_
        (\(Coyoneda f _ fieldSchema) ->
          case fieldSchema of
            Types.MandatoryFieldSchema name schema ->
              HashMap.singleton name
              $ Encode.fieldWith f
              $ querySchemaToEncoder schema

            Types.OptionalFieldSchema name schema ->
              HashMap.singleton name
              $ Encode.optionalFieldWith f
              $ querySchemaToEncoder schema
        )
        fields

querySchemaToQuery :: Types.QuerySchema a b -> Decode.Query b
querySchemaToQuery querySchema =
  Reflection.withTypeable (Types.querySchema_type querySchema) $
    case Types.querySchema_schema querySchema of
      Right schema -> Decode.queryWith $ schemaToDecoder schema
      Left _ -> Decode.undecodableQuery

schemaToDecoder :: Types.Schema a b -> Decode.Decoder b
schemaToDecoder (Types.Schema (Coyoneda _ f schemaBase)) =
  f <$> schemaBaseToDecoder schemaBase

schemaBaseToDecoder :: Types.SchemaBase a b -> Decode.Decoder b
schemaBaseToDecoder = \case
  Types.BoolSchema ->
    Decode.bool

  Types.NumberSchema ->
    Decode.number

  Types.StringSchema ->
    Decode.string

  Types.NullableSchema schema ->
    Decode.nullableWith $ schemaToDecoder schema

  Types.ArraySchema schema ->
    Decode.arrayWith $ querySchemaToQuery schema

  Types.StringMapSchema schema ->
    Decode.stringMapWith $ querySchemaToQuery schema

  Types.EnumSchema items ->
    Decode.enumWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(Types.ItemSchema name value) (SOP.Fn mkEnum) -> SOP.K (name, SOP.unK (mkEnum value)))
        items
        SOP.injections

  Types.VariantSchema constructors ->
    Decode.variantWith $ HashMap.fromList $ SOP.hcollapse $
      SOP.hzipWith
        (\(Types.ConstructorSchema name (Coyoneda _ lift schema)) (SOP.Fn construct) -> do
          let query = querySchemaToQuery schema
          let mkValue = SOP.unK . construct . lift
          SOP.K (name, Decode.constructorWith query mkValue)
        )
        constructors
        SOP.injections

  Types.RecordSchema fields ->
    Decode.recordWith $
      runAp
        (\(Coyoneda _ g fieldSchema) ->
          case fieldSchema of
            Types.MandatoryFieldSchema name schema ->
              fmap g $ Decode.fieldWith name $ querySchemaToQuery schema

            Types.OptionalFieldSchema name schema ->
              fmap g $ Decode.optionalFieldWith name $ querySchemaToQuery schema
        )
        (Types.unFieldsSchema fields)
