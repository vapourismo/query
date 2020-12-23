{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Query.Generic.Class
  ( SchemaFlavour (..) )
where

import           Data.Kind (Constraint, Type)
import           Data.Profunctor (Profunctor)
import qualified Data.SOP as SOP
import           Data.Text (Text)
import qualified Type.Reflection as Reflection

class
  ( forall x. Applicative (FieldsSchema cls x)
  , Profunctor (FieldsSchema cls)
  )
  => SchemaFlavour (cls :: Type -> Constraint)
  where
    data QuerySchema cls :: Type -> Type

    data Schema cls :: Type -> Type

    data ItemSchema cls :: forall k. (k -> Type) -> k -> Type

    data ConstructorSchema cls :: forall k. (k -> Type) -> k -> Type

    data FieldsSchema cls :: Type -> Type -> Type

    querySchema
      :: cls a
      => QuerySchema cls a

    querySchemaWith
      :: Reflection.Typeable a
      => Schema cls a
      -> QuerySchema cls a

    schema
      :: cls a
      => Schema cls a

    mapSchema
      :: (b -> a)
      -> (a -> b)
      -> Schema cls a
      -> Schema cls b

    enumWith
      :: SOP.SListI xss
      => SOP.NP (ItemSchema cls f) xss
      -> Schema cls (SOP.NS f xss)

    variantWith
      :: SOP.SListI xss
      => SOP.NP (ConstructorSchema cls f) xss
      -> Schema cls (SOP.NS f xss)

    recordWith
      :: FieldsSchema cls (SOP.NP SOP.I xs) (SOP.NP SOP.I xs)
      -> Schema cls (SOP.NP SOP.I xs)

    fieldWith
      :: Text
      -> QuerySchema cls b
      -> (a -> b)
      -> FieldsSchema cls a b

    optionalFieldWith
      :: Text
      -> QuerySchema cls b
      -> (a -> Maybe b)
      -> FieldsSchema cls a (Maybe b)

    item
      :: Text
      -> f a
      -> ItemSchema cls f a

    constructorWith
      :: Text
      -> (f a -> x)
      -> (x -> f a)
      -> QuerySchema cls x
      -> ConstructorSchema cls f a
