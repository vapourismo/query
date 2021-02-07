{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query
  ( runQuery
  , runQueryWith

  , TopLevelQuery (..)
  , runTopLevelQuery
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Decode.JSON as Decode.JSON
import qualified Data.Query.Evaluate as Evaluate
import qualified Data.Query.Resolver as Resolver
import qualified Data.Query.Shape as Shape
import qualified Data.Query.Types as Types
import qualified Data.Query.Value as Value

runQuery
  :: (Monad m, Decode.HasDecoder a)
  => Resolver.Resolver m
  -> Value.Value
  -> Either (Types.Located Evaluate.EvaluateError) (m a)
runQuery resolver =
  runQueryWith resolver Decode.query

runQueryWith
  :: Monad m
  => Resolver.Resolver m
  -> Decode.Query a
  -> Value.Value
  -> Either (Types.Located Evaluate.EvaluateError) (m a)
runQueryWith resolver query queryValue =
  Evaluate.runEvaluate resolver $ Evaluate.evalQuery query queryValue

data TopLevelQuery = TopLevelQuery
  { topLevelQuery_query  :: Value.CallValue
  , topLevelQuery_return :: Maybe Shape.Shape
  }
  deriving Show

instance Aeson.FromJSON TopLevelQuery where
  parseJSON = Aeson.withObject "TopLevelQuery" $ \object -> do
    return <- object Aeson..:? "return"
    TopLevelQuery
      <$> object Aeson..: "query"
      <*> traverse Decode.JSON.parseJson return

runTopLevelQuery
  :: Monad m
  => Resolver.Resolver m
  -> TopLevelQuery
  -> Either (Types.Located Evaluate.EvaluateError) (m Aeson.Value)
runTopLevelQuery resolver (TopLevelQuery call shape) =
  Evaluate.runEvaluate resolver $ Evaluate.evalTopLevelQuery call shape
