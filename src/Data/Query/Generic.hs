{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Query.Generic
  ( -- * Generic-powered schemas, encoders and decoders
    GHas
  , gSchema

  , GHasFields
  , gFieldsSchema

  , Class.SchemaFlavour (..)

    -- * Options
  , Options.Options (..)
  , Options.defaultOptions
  , Options.KnownGenericOption
  , Options.demoteOptions

    -- * Helpers for deriving-via
  , Generic
  , CustomGeneric (..)

    -- * Known type-level options to be used with 'CustomGeneric'
  , Options.CamelCaseFields
  , Options.TrimFieldTillUnderscore
  , Options.TrimFieldPrefix
  , Options.TrimVariantConstructorPrefix
  , Options.TrimEnumItemPrefix
  )
where

import           Data.Kind (Type)
import           Data.Profunctor (Profunctor (dimap))
import qualified Data.Query.Generic.Class as Class
import qualified Data.Query.Generic.Options as Options
import qualified Data.SOP as SOP
import qualified Data.SOP.NP as NP
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.SOP as Generics
import qualified Generics.SOP.Type.Metadata as Meta
import           Numeric.Natural (Natural)

-- * Utilities

indices
  :: forall xs
  .  SOP.SListI xs
  => SOP.NP (SOP.K Natural) xs
indices =
  case (SOP.hpure SOP.Proxy :: SOP.NP SOP.Proxy xs) of
    SOP.Nil    -> SOP.Nil
    _ SOP.:* _ -> SOP.K 0 SOP.:* SOP.hmap (SOP.mapKK (+ 1)) indices

-- * Enums

isEnum
  :: (SOP.SListI xss, Class.SchemaFlavour cls)
  => Options.Options
  -> SOP.NP Generics.ConstructorInfo xss
  -> Maybe (SOP.NP (Class.ItemSchema cls (SOP.NP SOP.I)) xss)
isEnum options = SOP.htraverse' $ \case
  (Generics.Constructor name :: Generics.ConstructorInfo xs)
    | SOP.Nil <- (SOP.hpure SOP.Proxy :: SOP.NP SOP.Proxy xs) ->
      Just $ Class.item (Options.enumItemModifier options (Text.pack name)) SOP.Nil

  _ -> Nothing

-- * Generic selectors

class GSelector cls field where
  gSelector
    :: Class.SchemaFlavour cls
    => Text
    -> (a -> field)
    -> Class.FieldsSchema cls a field

instance cls a => GSelector cls (Maybe a) where
  gSelector name access = Class.optionalFieldWith name Class.querySchema access

instance {-# OVERLAPPABLE #-} cls a => GSelector cls a where
  gSelector name access = Class.fieldWith name Class.querySchema access

-- * Generic constructors

class GConstructor cls (constructor :: Meta.ConstructorInfo) xs where
  gConstructor
    :: Class.SchemaFlavour cls
    => proxy constructor
    -> Options.Options
    -> Generics.ConstructorInfo xs
    -> Class.Schema cls (SOP.NP SOP.I xs)

instance
  SOP.All (GSelector cls) xs
  => GConstructor cls ('Meta.Record _c _fields) xs
  where
    gConstructor _ options (Generics.Record _ fieldInfo) =
      Class.recordWith
      $ SOP.hsequence
      $ SOP.hczipWith
          (SOP.Proxy :: SOP.Proxy (GSelector cls))
          (\(Generics.FieldInfo name) (SOP.Fn project) ->
            gSelector
              (Options.fieldNameModifier options (Text.pack name))
              (SOP.unI . project . SOP.K)
          )
          fieldInfo
          SOP.projections

    gConstructor _ _ _ = error "Impossible"

instance
  {-# OVERLAPPABLE #-}
  SOP.All (GSelector cls) xs
  => GConstructor cls ('Meta.Constructor _c) xs
  where
    gConstructor _ options Generics.Constructor{} =
      Class.recordWith
      $ SOP.hsequence
      $ SOP.hczipWith
          (SOP.Proxy :: SOP.Proxy (GSelector cls))
          (\(SOP.K index) (SOP.Fn project) ->
            gSelector
              (Options.fieldNameModifier options ("_" <> Text.pack (show index)))
              (SOP.unI . project . SOP.K)
          )
          indices
          SOP.projections

    gConstructor _ _ _ = error "Impossible"

instance {-# OVERLAPPING #-} cls x => GConstructor cls ('Meta.Constructor _c) '[x] where
  gConstructor _ _options Generics.Constructor{} =
    Class.mapSchema (SOP.unI . SOP.hd) (\x -> SOP.I x SOP.:* SOP.Nil) Class.schema

  gConstructor _ _ _ = error "Impossible"

instance
  {-# OVERLAPPING #-}
  ( Class.SchemaFlavour cls
  , cls ()
  )
  => GConstructor cls ('Meta.Constructor _c) '[]
  where
    gConstructor _ _options Generics.Constructor{} =
      Class.mapSchema (const ()) (const SOP.Nil) Class.schema

    gConstructor _ _ _ = error "Impossible"

instance SOP.All (GSelector cls) xs => GConstructor cls ('Meta.Infix _c _a _f) xs where
  gConstructor _ options Generics.Infix{} =
    Class.recordWith
    $ SOP.hsequence
    $ SOP.hczipWith
        (SOP.Proxy :: SOP.Proxy (GSelector cls))
        (\(SOP.K name) (SOP.Fn project) ->
          gSelector
            (Options.fieldNameModifier options name)
            (SOP.unI . project . SOP.K)
        )
        (SOP.K "lhs" SOP.:* SOP.K "rhs" SOP.:* SOP.Nil)
        SOP.projections

  gConstructor _ _ _ = error "Impossible"

-- * Generic datatypes

class GDatatype cls (info :: Meta.DatatypeInfo) (xss :: [[Type]]) where
  gDatatype
    :: Class.SchemaFlavour cls
    => proxy info
    -> Options.Options
    -> Generics.DatatypeInfo xss
    -> Class.Schema cls (SOP.SOP SOP.I xss)

newtype ConstructorApplication cls xs = ConstructorApplication
  (Generics.ConstructorInfo xs -> Class.Schema cls (SOP.NP SOP.I xs))

instance
  {-# OVERLAPPABLE #-}
  ( SOP.AllZip (GConstructor cls) constructors xss
  , SOP.All2 (Class.SupplementalClass cls) xss
  )
  => GDatatype cls ('Meta.ADT _m _d constructors _s) xss
  where
    gDatatype _ options (Generics.ADT _ _ constructors _) =
      Class.mapSchema SOP.unSOP SOP.SOP $
        case isEnum options constructors of
          Just items ->
            Class.enumWith items

          _ ->
            Class.variantWith $
              SOP.hczipWith
                (SOP.Proxy :: SOP.Proxy (SOP.All (Class.SupplementalClass cls)))
                (\(ConstructorApplication run) (info :: Generics.ConstructorInfo xs) ->
                  Class.constructorWith (Text.pack (Generics.constructorName info)) id id
                  $ Class.querySchemaWith
                  $ run info
                )
                (NP.trans_NP
                  (SOP.Proxy :: SOP.Proxy (GConstructor cls))
                  (\info -> ConstructorApplication $ gConstructor info options)
                  (SOP.hpure SOP.Proxy :: SOP.NP SOP.Proxy constructors)
                :: SOP.NP (ConstructorApplication cls) xss
                )
                constructors

    gDatatype _ _ _ = error "Impossible"

instance
  {-# OVERLAPPING #-}
  GConstructor cls constructor xs
  => GDatatype cls ('Meta.ADT _m _d '[constructor] _s) '[xs]
  where
    gDatatype _ options (Generics.ADT _ _ constructors _) =
      Class.mapSchema (SOP.unZ . SOP.unSOP) (SOP.SOP . SOP.Z) $
        gConstructor @_ @constructor SOP.Proxy options (SOP.hd constructors)

    gDatatype _ _ _ = error "Impossible"

instance
  GConstructor cls constructor xs
  => GDatatype cls ('Meta.Newtype _m _d constructor) '[xs]
  where
    gDatatype _ options (Generics.Newtype _ _ constructor) =
      Class.mapSchema (SOP.unZ . SOP.unSOP) (SOP.SOP . SOP.Z) $
        gConstructor @_ @constructor SOP.Proxy options constructor

    gDatatype _ _ _ = error "Impossible"

-- * Generic fields

class GFieldsDatatype cls (info :: Meta.DatatypeInfo) (xss :: [[Type]]) where
  gFieldsDatatype
    :: Class.SchemaFlavour cls
    => proxy info
    -> Options.Options
    -> Generics.DatatypeInfo xss
    -> Class.FieldsSchema cls (SOP.SOP SOP.I xss) (SOP.SOP SOP.I xss)

instance
  SOP.All (GSelector cls) xs
  => GFieldsDatatype cls ('Meta.ADT _m _d '[ 'Meta.Record _c _f ] _s) '[xs]
  where
    gFieldsDatatype _ options info =
      case info of
        Generics.ADT _ _ (SOP.hd -> Generics.Record _ fieldInfo) _ ->
          dimap (SOP.unZ . SOP.unSOP) (SOP.SOP . SOP.Z) $
            SOP.hsequence $
              SOP.hczipWith
                (SOP.Proxy :: SOP.Proxy (GSelector cls))
                (\(Generics.FieldInfo name) (SOP.Fn project) ->
                  gSelector
                    (Options.fieldNameModifier options (Text.pack name))
                    (SOP.unI . project . SOP.K)
                )
                fieldInfo
                SOP.projections

        _ -> error "Impossible"

-- * Generic schemas

type GHas cls a =
  ( Generics.Generic a
  , Generics.HasDatatypeInfo a
  , Class.SchemaFlavour cls
  , GDatatype cls (Generics.DatatypeInfoOf a) (Generics.Code a)
  )

gSchema
  :: forall a cls
  .  GHas cls a
  => Options.Options
  -> Class.Schema cls a
gSchema options =
  Class.mapSchema Generics.from Generics.to $
    gDatatype
      (SOP.Proxy :: SOP.Proxy (Generics.DatatypeInfoOf a))
      options
      (Generics.datatypeInfo @a SOP.Proxy)

type GHasFields cls a =
  ( Generics.Generic a
  , Generics.HasDatatypeInfo a
  , Class.SchemaFlavour cls
  , GFieldsDatatype cls (Generics.DatatypeInfoOf a) (Generics.Code a)
  )

gFieldsSchema
  :: forall a cls
  .  GHasFields cls a
  => Options.Options
  -> Class.FieldsSchema cls a a
gFieldsSchema options =
  dimap Generics.from Generics.to $
    gFieldsDatatype
      (SOP.Proxy :: SOP.Proxy (Generics.DatatypeInfoOf a))
      options
      (Generics.datatypeInfo @a SOP.Proxy)

-- * Generic type for deriving-via

-- | Target for deriving-via with some options.
newtype CustomGeneric (options :: [k]) a = CustomGeneric
  { fromCustomGeneric :: a }

-- | Target for deriving-via using 'Options.defaultOptions'.
type Generic = CustomGeneric ('[] :: [Type])
