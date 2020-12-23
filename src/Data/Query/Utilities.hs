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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Query.Utilities
  ( ContravariantCoyonedaShow (..)
  , CoyonedaShow (..)
  , foldTypeRep
  , typeReps
  , ApCoyoneda (..)
  , PluggableShow (..)
  , PluggableShowEnv (..)
  , FunctorShow1 (..)
  , customLiftShowsPrec
  )
where

import           Control.Applicative.Free (Ap, hoistAp)
import           Data.Coerce (Coercible, coerce)
import qualified Data.Functor.Classes as Functor
import qualified Data.Functor.Contravariant.Coyoneda as Contravariant
import qualified Data.Functor.Coyoneda as Functor
import           Data.Profunctor (Profunctor (..))
import qualified Data.Profunctor.Yoneda as Profunctor
import qualified Data.Reflection as Reflection
import qualified Data.SOP as SOP
import qualified Prettyprinter as Pretty
import qualified Type.Reflection as Reflection

newtype ContravariantCoyonedaShow f a = ContravariantCoyonedaShow (Contravariant.Coyoneda f a)

instance (forall x. Show (f x)) => Show (ContravariantCoyonedaShow f a) where
  show (ContravariantCoyonedaShow (Contravariant.Coyoneda _ item)) = show item

instance (forall x. Pretty.Pretty (f x)) => Pretty.Pretty (ContravariantCoyonedaShow f a) where
  pretty (ContravariantCoyonedaShow (Contravariant.Coyoneda _ item)) = Pretty.pretty item

newtype CoyonedaShow f a = CoyonedaShow (Functor.Coyoneda f a)

instance (forall x. Show (f x)) => Show (CoyonedaShow f a) where
  show (CoyonedaShow (Functor.Coyoneda _ item)) = show item

instance (forall x. Pretty.Pretty (f x)) => Pretty.Pretty (CoyonedaShow f a) where
  pretty (CoyonedaShow (Functor.Coyoneda _ item)) = Pretty.pretty item

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

newtype PluggableShow a = PluggableShow
  { unPluggableShow :: a }

data PluggableShowEnv a = PluggableShowEnv
  { pluggableShowEnv_showsPrec :: Int -> PluggableShow a -> ShowS
  , pluggableShowEnv_showList :: [PluggableShow a] -> ShowS
  }

instance Reflection.Given (PluggableShowEnv a) => Show (PluggableShow a) where
  showsPrec = pluggableShowEnv_showsPrec Reflection.given

  showList = pluggableShowEnv_showList Reflection.given

newtype FunctorShow1 f a = FunctorShow1 (f a)

instance
  ( (forall x. Coercible (f x) (f (PluggableShow x)))
  , (forall x. (Show x => Show (f x)))
  )
  => Functor.Show1 (FunctorShow1 f)
  where
    liftShowsPrec p l n (FunctorShow1 i) = customLiftShowsPrec p l n i

customLiftShowsPrec
  :: ( Coercible (f a) (f (PluggableShow a))
     , (forall x. (Show x => Show (f x)))
     )
  => (Int -> a -> ShowS)
  -> ([a] -> ShowS)
  -> Int
  -> f a
  -> ShowS
customLiftShowsPrec envShowsPrec envShowList prec (shape :: f a) =
  Reflection.give env showsPrec prec (coerce shape :: f (PluggableShow a))
  where
    env :: PluggableShowEnv a
    env = PluggableShowEnv
      { pluggableShowEnv_showsPrec = coerce envShowsPrec
      , pluggableShowEnv_showList = coerce envShowList
      }
