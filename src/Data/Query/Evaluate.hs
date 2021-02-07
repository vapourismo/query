{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query.Evaluate
  ( Evaluate
  , runEvaluate
  , evalQuery
  , evalTopLevelQuery

  , EvaluateError (..)
  )
where

import           Control.Monad (join)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (Bifunctor (first))
import           Data.Functor.Compose (Compose (Compose))
import qualified Data.Query.Decode.JSON as Decode
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Resolver as Resolver
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Types as Types
import qualified Data.Query.Value as Value

data EvaluateError
  = DecodeError Decode.DecodeError
  | ResolveError Resolver.ResolveError
  deriving Show

type InnerEvaluate m = Reader.ReaderT (Resolver.Resolver m) (Either (Types.Located EvaluateError))

newtype Evaluate m a = Evaluate
  { unEvaluate :: InnerEvaluate m (m a) }
  deriving (Functor, Applicative) via Compose (InnerEvaluate m) m

instance Trans.MonadTrans Evaluate where
  lift = Evaluate . pure

instance Monad m => Decode.CanCallFunction (Evaluate m) where
  callFunction name retType args = Evaluate $ Reader.ReaderT $ \resolver -> do
    decoder <- first (Types.Located [] . ResolveError) $
      Resolver.resolveFunction resolver name retType
    call <- first (fmap DecodeError) $
      Decode.runDecoded $ Decode.evalFieldDecoder decoder args
    join <$> Reader.runReaderT (unEvaluate call) resolver

runEvaluate :: Resolver.Resolver m -> Evaluate m a -> Either (Types.Located EvaluateError) (m a)
runEvaluate resolver evaluate =
  Reader.runReaderT (unEvaluate evaluate) resolver

evalQuery :: Monad m => Decode.Query a -> Value.Value -> Evaluate m a
evalQuery query value =
  case Decode.runDecoded (Decode.evalQuery query value) of
    Left error -> Evaluate $ Reader.lift $ Left $ fmap DecodeError error
    Right call -> call

evalTopLevelQuery :: Monad m => Value.CallValue -> Maybe Shape.Shape -> Evaluate m Aeson.Value
evalTopLevelQuery (Value.CallValue name args) shape = Evaluate $ Reader.ReaderT $ \resolver -> do
  decoder <- first (Types.Located [] . ResolveError) $
    Resolver.resolveTopLevelFunction resolver name shape
  call <- first (fmap DecodeError) $
    Decode.runDecoded $ Decode.evalFieldDecoder decoder args
  join <$> Reader.runReaderT (unEvaluate call) resolver
