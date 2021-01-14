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
  , enum
  , enumWith

    -- ** Items
  , Types.ItemEncoder
  , item

    -- * Variants
  , variantWith

    -- ** Constructors
  , Types.ConstructorEncoder
  , contramapConstructorEncoder
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
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (fromMaybe)
import           Data.Profunctor (Profunctor (..))
import qualified Data.Profunctor.Yoneda as Profunctor
import qualified Data.Query.Encode.Types as Types
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

-- | @a@ can be encoded via 'Types.Encoder'
--
-- When defining an instance, you can omit 'encoder' if @a@ has instances of SOP generics
-- (@generics-sop@).
--
class HasEncoder a where
  -- | 'Types.Encoder' for @a@
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

-- | Boolean encoder
bool :: Types.Encoder Bool
bool = liftBase Types.BoolEncoder

-- | Number encoder
number :: Types.Encoder Scientific
number = liftBase Types.NumberEncoder

-- | String encoder
string :: Types.Encoder Text
string = liftBase Types.StringEncoder

-- | See 'nullableWith'.
nullable :: HasEncoder a => Types.Encoder (Maybe a)
nullable = nullableWith encoder

-- | Encode an optional @a@. In case of @Nothing@, it will encode to @null@. Otherwise the given
-- 'Types.Encoder' will be used.
nullableWith :: Types.Encoder a -> Types.Encoder (Maybe a)
nullableWith = liftBase . Types.NullableEncoder

-- | See 'arrayWith'.
array :: HasEncoder a => Types.Encoder (Vector.Vector a)
array = arrayWith encoder

-- | Encode as an array where its items will be encoded using the given 'Types.Encoder'.
arrayWith :: Types.Encoder a -> Types.Encoder (Vector.Vector a)
arrayWith = liftBase . Types.ArrayEncoder

-- | See 'stringMapWith'.
stringMap :: HasEncoder a => Types.Encoder (HashMap.HashMap Text a)
stringMap = stringMapWith encoder

-- | Encode as an object mapping strings to values that are encoded using the given 'Types.Encoder'.
stringMapWith :: Types.Encoder a -> Types.Encoder (HashMap.HashMap Text a)
stringMapWith = liftBase . Types.StringMapEncoder

-- | Encode an enum.
--
-- Note: While in practise this encoder isn't partial, a bad 'Enum' implementation can make it so.
--
enum :: forall a. (Bounded a, Enum a, Show a) => Types.Encoder a
enum =
  Utilities.instantiateProduct [minBound .. maxBound @a] $ \product ->
    let
      items = SOP.hmap (item . Text.pack . show . SOP.unK) product

      valueMap =
        IntMap.fromList $ SOP.hcollapse $
          SOP.hzipWith
            (\(SOP.K value) (SOP.Fn f) ->
              SOP.K (fromEnum value, SOP.unK (f SOP.Proxy))
            )
            product
            SOP.injections

      findSum value =
        fromMaybe (error "Unknown enum value") $
          IntMap.lookup (fromEnum value) valueMap

    in
      contramap findSum (enumWith items)

-- | Encode an enum which is made up of items described in the given n-ary product.
enumWith
  :: SOP.SListI xs
  => SOP.NP Types.ItemEncoder xs
  -> Types.Encoder (SOP.NS f xs)
enumWith =
  liftBase . Types.EnumEncoder

-- | Describe an item of an enum.
item
  :: Text -- ^ Enum value
  -> Types.ItemEncoder a
item =
  Types.ItemEncoder

-- | Encode as a variant. The constructors of the variant are described via the given n-ary product.
variantWith
  :: SOP.SListI xs
  => SOP.NP (Types.ConstructorEncoder f) xs
  -> Types.Encoder (SOP.NS f xs)
variantWith =
  liftBase . Types.VariantEncoder

-- | Transform the given 'Types.ConstructorEncoder'.
contramapConstructorEncoder
  :: (g b -> f a)
  -> Types.ConstructorEncoder f a
  -> Types.ConstructorEncoder g b
contramapConstructorEncoder f encoder = Types.ConstructorEncoder
  { Types.constructorEncoder_name = Types.constructorEncoder_name encoder
  , Types.constructorEncoder_value = contramap f $ Types.constructorEncoder_value encoder
  }

-- | See 'constructorWith'.
constructor
  :: HasEncoder a
  => Text -- ^ Constructor name
  -> Types.ConstructorEncoder SOP.I a
constructor name =
  Types.ConstructorEncoder name $ contramap SOP.unI encoder

-- | Encoder for a constructor of a variant
constructorWith
  :: Text -- ^ Constructor name
  -> Types.Encoder (f a) -- ^ Encoder for constructor value
  -> Types.ConstructorEncoder f a
constructorWith =
  Types.ConstructorEncoder

-- | See 'fieldWith'.
field
  :: HasEncoder b
  => (a -> b) -- ^ Field accessor
  -> Types.FieldEncoder a
field access =
  fieldWith access encoder

-- | Field encoder for a field @b@ in type @a@
fieldWith
  :: (a -> b) -- ^ Field accessor
  -> Types.Encoder b -- ^ Encoder for the field value
  -> Types.FieldEncoder a
fieldWith access encoder =
  Types.FieldEncoder
  $ Contravariant.Coyoneda.liftCoyoneda
  $ Types.MandatoryFieldSelector
  $ contramap access encoder

-- | See 'optionalFieldWith'.
optionalField
  :: HasEncoder b
  => (a -> Maybe b) -- ^ Field accessor (@Just@ when present, @Nothing@ when absent)
  -> Types.FieldEncoder a
optionalField access =
  optionalFieldWith access encoder

-- | Field encoder for an optional type @b@ in type @a@.
optionalFieldWith
  :: (a -> Maybe b) -- ^ Field accessor (@Just@ when present, @Nothing@ when absent)
  -> Types.Encoder b -- ^ Encoder for the field when it is present
  -> Types.FieldEncoder a
optionalFieldWith access encoder =
  contramap access
  $ Types.FieldEncoder
  $ Contravariant.Coyoneda.liftCoyoneda
  $ Types.OptionalFieldSelector encoder

-- | See 'recordWith'.
record :: HasFieldsEncoder a => Types.Encoder a
record = recordWith fieldsEncoder

-- | Encode a record with the given fields.
recordWith
  :: HashMap.HashMap Text (Types.FieldEncoder a) -- ^ Mapping from field name to its encoder
  -> Types.Encoder a
recordWith =
  liftBase . Types.RecordEncoder

-- * Schema derivation

-- | See 'querySchemaEncoderWith'.
querySchemaEncoder :: Schema.HasSchema a => Types.Encoder a
querySchemaEncoder = querySchemaEncoderWith Schema.querySchema

-- | Convert a 'Schema.QuerySchema' to a 'Types.Encoder'.
querySchemaEncoderWith :: Schema.QuerySchema a b -> Types.Encoder a
querySchemaEncoderWith querySchema =
  case Schema.querySchema_schema querySchema of
    Left encoder -> encoder
    Right schema -> schemaEncoderWith schema

-- | See 'schemaEncoderWith'.
schemaEncoder :: Schema.HasSchema a => Types.Encoder a
schemaEncoder = schemaEncoderWith Schema.schema

-- | Convert a 'Schema.Schema' to a 'Types.Encoder'.
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
    GFieldsEncoder
    $ liftAp
    $ FieldEncoderWrap name
    $ fieldWith access
    $ unGQueryEncoder encoder

  optionalFieldWith name encoder access =
    GFieldsEncoder
    $ liftAp
    $ FieldEncoderWrap name
    $ optionalFieldWith access
    $ unGQueryEncoder encoder

  item name _ = GItemEncoder $ item name

  constructorWith name extract _lift (GQueryEncoder encoder) =
    GConstructorEncoder $ constructorWith name $ contramap extract encoder

-- | Generic 'Types.Encoder' for an @a@ that has instances for SOP generics
generic
  :: Generic.GHas HasEncoder a
  => Generic.Options
  -> Types.Encoder a
generic options =
  unGEncoder $ Generic.gSchema options

-- | Generic 'Types.FieldEncoder's for an @a@ that has instances for SOP generics.
genericFields
  :: Generic.GHasFields HasEncoder a
  => Generic.Options
  -> HashMap.HashMap Text (Types.FieldEncoder a)
genericFields options =
  runAp_
    (\(FieldEncoderWrap name encoder) -> HashMap.singleton name encoder)
    (unGFieldsEncoder (Generic.gFieldsSchema options))
