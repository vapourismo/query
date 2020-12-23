{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Query.Utilities
  ( ContravariantCoyonedaShow (..)
  , CoyonedaShow (..)
  , foldTypeRep
  , typeReps
  , ApCoyoneda (..)
  )
where

import           Control.Applicative.Free (Ap, hoistAp)
import qualified Data.Functor.Contravariant.Coyoneda as Contravariant
import qualified Data.Functor.Coyoneda as Functor
import           Data.Profunctor (Profunctor (..))
import qualified Data.Profunctor.Yoneda as Profunctor
import qualified Data.SOP as SOP
import qualified Type.Reflection as Reflection

newtype ContravariantCoyonedaShow f a = ContravariantCoyonedaShow (Contravariant.Coyoneda f a)

instance (forall x. Show (f x)) => Show (ContravariantCoyonedaShow f a) where
  show (ContravariantCoyonedaShow (Contravariant.Coyoneda _ item)) = show item

newtype CoyonedaShow f a = CoyonedaShow (Functor.Coyoneda f a)

instance (forall x. Show (f x)) => Show (CoyonedaShow f a) where
  show (CoyonedaShow (Functor.Coyoneda _ item)) = show item

foldTypeRep
  :: forall k (ys :: [k])
  .  Reflection.Typeable k
  => SOP.NP Reflection.TypeRep ys
  -> Reflection.TypeRep ys
foldTypeRep = \case
  SOP.Nil ->
    Reflection.typeRep

  typeRep SOP.:* tail ->
    Reflection.App (Reflection.App Reflection.typeRep typeRep) (foldTypeRep tail)

typeReps
  :: forall k (xs :: [k])
  .  (SOP.All Reflection.Typeable xs, Reflection.Typeable k)
  => Reflection.TypeRep xs
typeReps =
  foldTypeRep $
    SOP.hcpure
      (SOP.Proxy :: SOP.Proxy Reflection.Typeable)
      Reflection.typeRep

newtype ApCoyoneda f a b = ApCoyoneda
  { unApCoyoneda :: Ap (Profunctor.Coyoneda f a) b }
  deriving newtype (Functor, Applicative)

instance Profunctor (ApCoyoneda f) where
  lmap f = ApCoyoneda . hoistAp (lmap f) . unApCoyoneda

  rmap = fmap
