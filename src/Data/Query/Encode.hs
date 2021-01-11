{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Query.Encode
  ( Types.Encoder
  , HasEncoder (..)
  , generic

    -- * Primitives
  , bool
  , number
  , string

    -- * Nullables
  , nullable
  , nullableWith

    -- * Lists
  , array
  , arrayWith

    -- * String mappings
  , stringMap
  , stringMapWith

    -- * Enums
  , enumWith

    -- ** Items
  , Types.ItemEncoder
  , item

    -- * Variants
  , variantWith

    -- ** Constructors
  , Types.ConstructorEncoder
  , constructor
  , constructorWith

    -- * Records
  , record
  , recordWith

    -- ** Fields
  , Types.FieldEncoder
  , HasFieldsEncoder (..)
  , genericFields
  , field
  , fieldWith
  , optionalField
  , optionalFieldWith

  -- * Schema derivation
  , querySchemaEncoder
  , querySchemaEncoderWith
  , schemaEncoder
  , schemaEncoderWith
  )
where

import           Control.Applicative.Free (Ap, hoistAp, liftAp, runAp_)
import           Data.Coerce (coerce)
import           Data.Fix (Fix, unFix)
import           Data.Functor.Contravariant (Contravariant (contramap))
import qualified Data.Functor.Contravariant.Coyoneda as Contravariant.Coyoneda
import qualified Data.HashMap.Strict as HashMap
import           Data.Profunctor (Profunctor (..))
import qualified Data.Profunctor.Yoneda as Profunctor
import qualified Data.Query.Encode.Types as Types
import qualified Data.Query.Generic as Generic
import qualified Data.Query.Schema as Schema
import qualified Data.Query.Schema.Types as Schema
import qualified Data.Query.Shape as Shape
import qualified Data.SOP as SOP
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Type.Reflection as Reflection

-- * Classes

class HasEncoder a where
  encoder :: Types.Encoder a

  -- | Generic implemention
  default encoder :: Generic.GHas HasEncoder a => Types.Encoder a
  encoder = generic Generic.defaultOptions

instance HasEncoder () where
  encoder = record

instance HasEncoder Bool where
  encoder = bool

instance HasEncoder Scientific where
  encoder = number

instance HasEncoder Text where
  encoder = string

instance HasEncoder a => HasEncoder (Vector.Vector a) where
  encoder = array

instance HasEncoder a => HasEncoder [a] where
  encoder = contramap Vector.fromList array

instance HasEncoder a => HasEncoder (HashMap.HashMap Text a) where
  encoder = stringMap

instance HasEncoder (f (Fix f)) => HasEncoder (Fix f) where
  encoder = contramap unFix encoder

instance
  ( Generic.GHas HasEncoder a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasEncoder (Generic.CustomGeneric options a)
  where
    encoder = coerce (generic @a (Generic.demoteOptions @options SOP.Proxy))

instance HasEncoder a => HasEncoder (Shape.FieldShapeF a) where
  encoder = record

instance (HasEncoder a, Reflection.Typeable a) => HasEncoder (Shape.ShapeF a) where
  encoder = generic Generic.defaultOptions

class HasFieldsEncoder a where
  fieldsEncoder :: HashMap.HashMap Text (Types.FieldEncoder a)

  default fieldsEncoder
    :: Generic.GHasFields HasEncoder a
    => HashMap.HashMap Text (Types.FieldEncoder a)
  fieldsEncoder =
    genericFields Generic.defaultOptions

instance HasFieldsEncoder () where
  fieldsEncoder = HashMap.empty

instance
  ( Generic.GHasFields HasEncoder a
  , SOP.All Generic.KnownGenericOption options
  )
  => HasFieldsEncoder (Generic.CustomGeneric options a)
  where
    fieldsEncoder = coerce (genericFields @a (Generic.demoteOptions @options SOP.Proxy))

deriving
  via Generic.CustomGeneric '[Generic.TrimFieldTillUnderscore] (Shape.FieldShapeF a)
  instance HasEncoder a => HasFieldsEncoder (Shape.FieldShapeF a)

-- * Encoders

liftBase :: Types.EncoderBase a -> Types.Encoder a
liftBase = Types.Encoder . Contravariant.Coyoneda.liftCoyoneda

bool :: Types.Encoder Bool
bool = liftBase Types.BoolEncoder

number :: Types.Encoder Scientific
number = liftBase Types.NumberEncoder

string :: Types.Encoder Text
string = liftBase Types.StringEncoder

nullable :: HasEncoder a => Types.Encoder (Maybe a)
nullable = nullableWith encoder

nullableWith :: Types.Encoder a -> Types.Encoder (Maybe a)
nullableWith = liftBase . Types.NullableEncoder

array :: HasEncoder a => Types.Encoder (Vector.Vector a)
array = arrayWith encoder

arrayWith :: Types.Encoder a -> Types.Encoder (Vector.Vector a)
arrayWith = liftBase . Types.ArrayEncoder

stringMap :: HasEncoder a => Types.Encoder (HashMap.HashMap Text a)
stringMap = stringMapWith encoder

stringMapWith :: Types.Encoder a -> Types.Encoder (HashMap.HashMap Text a)
stringMapWith = liftBase . Types.StringMapEncoder

enumWith :: SOP.SListI xs => SOP.NP Types.ItemEncoder xs -> Types.Encoder (SOP.NS f xs)
enumWith = liftBase . Types.EnumEncoder

item :: Text -> Types.ItemEncoder a
item = Types.ItemEncoder

variantWith
  :: SOP.SListI xs
  => SOP.NP (Types.ConstructorEncoder f) xs
  -> Types.Encoder (SOP.NS f xs)
variantWith =
  liftBase . Types.VariantEncoder

constructor
  :: HasEncoder a
  => Text
  -> Types.ConstructorEncoder SOP.I a
constructor name =
  Types.ConstructorEncoder name $ contramap SOP.unI encoder

constructorWith
  :: Text
  -> Types.Encoder (f a)
  -> Types.ConstructorEncoder f a
constructorWith =
  Types.ConstructorEncoder

field :: HasEncoder b => (a -> b) -> Types.FieldEncoder a
field access = fieldWith access encoder

fieldWith :: (a -> b) -> Types.Encoder b -> Types.FieldEncoder a
fieldWith access encoder =
  Types.FieldEncoder
    $ Contravariant.Coyoneda.liftCoyoneda
    $ Types.MandatoryFieldSelector
    $ contramap access encoder

optionalField :: HasEncoder b => (a -> Maybe b) -> Types.FieldEncoder a
optionalField access = optionalFieldWith access encoder

optionalFieldWith :: (a -> Maybe b) -> Types.Encoder b -> Types.FieldEncoder a
optionalFieldWith access encoder =
  contramap access
    $ Types.FieldEncoder
    $ Contravariant.Coyoneda.liftCoyoneda
    $ Types.OptionalFieldSelector encoder

record :: HasFieldsEncoder a => Types.Encoder a
record = recordWith fieldsEncoder

recordWith :: HashMap.HashMap Text (Types.FieldEncoder a) -> Types.Encoder a
recordWith = liftBase . Types.RecordEncoder

-- * Schema derivation

querySchemaEncoder :: Schema.HasSchema a => Types.Encoder a
querySchemaEncoder = querySchemaEncoderWith Schema.querySchema

querySchemaEncoderWith :: Schema.QuerySchema a b -> Types.Encoder a
querySchemaEncoderWith querySchema =
  case Schema.querySchema_schema querySchema of
    Left encoder -> encoder
    Right schema -> schemaEncoderWith schema

schemaEncoder :: Schema.HasSchema a => Types.Encoder a
schemaEncoder = schemaEncoderWith Schema.schema

schemaEncoderWith :: Schema.Schema a b -> Types.Encoder a
schemaEncoderWith (Schema.Schema (Profunctor.Coyoneda f _ schemaBase)) =
  contramap f $ schemaBaseEncoderWith schemaBase

schemaBaseEncoderWith :: Schema.SchemaBase a b -> Types.Encoder a
schemaBaseEncoderWith = \case
  Schema.BoolSchema ->
    bool

  Schema.NumberSchema ->
    number

  Schema.StringSchema ->
    string

  Schema.NullableSchema schema ->
    nullableWith $ schemaEncoderWith schema

  Schema.ArraySchema schema ->
    arrayWith $ querySchemaEncoderWith schema

  Schema.StringMapSchema schema ->
    stringMapWith $ querySchemaEncoderWith schema

  Schema.EnumSchema items ->
    enumWith $
      SOP.hmap
        (\(Schema.ItemSchema name _) -> item name)
        items

  Schema.VariantSchema constructors ->
    variantWith $
      SOP.hmap
        (\(Schema.ConstructorSchema name (Profunctor.Coyoneda extract _ schema)) ->
          constructorWith name (contramap extract (querySchemaEncoderWith schema))
        )
        constructors

  Schema.RecordSchema (Schema.FieldsSchema fields) ->
    recordWith $
      runAp_
        (\(Profunctor.Coyoneda f _ fieldSchema) ->
          case fieldSchema of
            Schema.MandatoryFieldSchema name schema ->
              HashMap.singleton name
              $ fieldWith f
              $ querySchemaEncoderWith schema

            Schema.OptionalFieldSchema name schema ->
              HashMap.singleton name
              $ optionalFieldWith f
              $ querySchemaEncoderWith schema
        )
        fields

-- * Generics

data FieldEncoderWrap a b where
  FieldEncoderWrap
    :: Text
    -> Types.FieldEncoder a
    -> FieldEncoderWrap a b

deriving
  via Ap (FieldEncoderWrap a)
  instance Functor (Generic.FieldsSchema HasEncoder a)

deriving
  via Ap (FieldEncoderWrap a)
  instance Applicative (Generic.FieldsSchema HasEncoder a)

instance Profunctor (Generic.FieldsSchema HasEncoder) where
  lmap f (GFieldsEncoder ap) =
    GFieldsEncoder $
      hoistAp
        (\(FieldEncoderWrap name encoder) -> FieldEncoderWrap name (contramap f encoder))
        ap

  rmap = fmap

instance Generic.SchemaFlavour HasEncoder where
  newtype QuerySchema HasEncoder a = GQueryEncoder
    { unGQueryEncoder :: Types.Encoder a }

  newtype Schema HasEncoder a = GEncoder
    { unGEncoder :: Types.Encoder a }

  newtype ItemSchema HasEncoder _ a = GItemEncoder
    { unGItemEncoder :: Types.ItemEncoder a }

  newtype ConstructorSchema HasEncoder f a = GConstructorEncoder
    { unGConstructorEncoder :: Types.ConstructorEncoder f a }

  newtype FieldsSchema HasEncoder a b = GFieldsEncoder
    { unGFieldsEncoder :: Ap (FieldEncoderWrap a) b }

  type SupplementalClass HasEncoder = SOP.Top

  querySchema = GQueryEncoder encoder

  querySchemaWith = GQueryEncoder . unGEncoder

  schema = GEncoder encoder

  mapSchema f _ (GEncoder a) = GEncoder $ contramap f a

  enumWith = GEncoder . enumWith . SOP.hmap unGItemEncoder

  variantWith = GEncoder . variantWith . SOP.hmap unGConstructorEncoder

  recordWith fields =
    GEncoder $ recordWith $
      runAp_
        (\(FieldEncoderWrap name encoder) -> HashMap.singleton name encoder)
        (unGFieldsEncoder fields)

  fieldWith name encoder access =
    GFieldsEncoder $ liftAp $ FieldEncoderWrap name $ fieldWith access $ unGQueryEncoder encoder

  optionalFieldWith name encoder access =
    GFieldsEncoder $ liftAp $ FieldEncoderWrap name $ optionalFieldWith access $ unGQueryEncoder encoder

  item name _ = GItemEncoder $ item name

  constructorWith name extract _lift (GQueryEncoder encoder) =
    GConstructorEncoder $ constructorWith name $ contramap extract encoder

-- | Encoder for a generic type @a@
generic
  :: Generic.GHas HasEncoder a
  => Generic.Options
  -> Types.Encoder a
generic options =
  unGEncoder $ Generic.gSchema options

-- | Field encoder for a generic record type @a@
genericFields
  :: Generic.GHasFields HasEncoder a
  => Generic.Options
  -> HashMap.HashMap Text (Types.FieldEncoder a)
genericFields options =
  runAp_
    (\(FieldEncoderWrap name encoder) -> HashMap.singleton name encoder)
    (unGFieldsEncoder (Generic.gFieldsSchema options))
