{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query
  ( evalQuery
  , evalQueryWith

  , Evaluate.Resolver
  , mkResolver

  , TopLevelQuery (..)
  , runTopLevelQuery

  , TopLevelQueryError (..)
  , TopLevelMatchError (..)
  )
where

import qualified Data.Aeson as Aeson
import           Data.Bifunctor (Bifunctor (first))
import           Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode as Decode
import qualified Data.Query.Decode.JSON as Decode.JSON
import qualified Data.Query.Encode.JSON as Encode.JSON
import qualified Data.Query.Encode.Projection as Projection
import qualified Data.Query.Evaluate as Evaluate
import           Data.Query.Function (Function (Function))
import qualified Data.Query.Schema.Types as Schema
import qualified Data.Query.Value as Value
import           Data.Text (Text)
import qualified Type.Reflection as Reflection

---

data Functions m = Functions
  { functionsByType    :: HashMap.HashMap Reflection.SomeTypeRep (Function m)
  , functionsByEncoder :: [Function m]
  }

combineFunctions :: Functions m -> Functions m -> Functions m
combineFunctions lhs rhs = Functions
  { functionsByType    = functionsByType lhs <> functionsByType rhs
  , functionsByEncoder = functionsByEncoder lhs <> functionsByEncoder rhs
  }

---

mkResolver
  :: forall m
  .  (Monad m, Reflection.Typeable m)
  => [Function m]
  -> Evaluate.Resolver m
mkResolver funs =
  Evaluate.Resolver resolve resolveTopLevel
  where
    funItem
      :: Function m
      -> HashMap.HashMap Text (Functions m)
    funItem fun@(Function name _decode returnType _encode _call) =
      HashMap.singleton name Functions
        { functionsByType    = HashMap.singleton (Reflection.SomeTypeRep returnType) fun
        , functionsByEncoder = [fun]
        }

    allFuns = foldr (HashMap.unionWith combineFunctions . funItem) HashMap.empty funs

    resolve :: Reflection.TypeRep a -> Text -> Value.Object -> Evaluate.Evaluate m a
    resolve targetTypeRep name args = do
      let targetTyp = Reflection.SomeTypeRep targetTypeRep
      case HashMap.lookup name allFuns of
        Just functions ->
          case HashMap.lookup targetTyp (functionsByType functions) of
            Just (Function _ decode returnType _ call)
              | Just Reflection.HRefl <- Reflection.eqTypeRep returnType targetTypeRep ->
                Evaluate.withEvaluate (>>= call) (Decode.JSON.evalFieldDecoder decode args)

            _ ->
              Evaluate.throwQueryError
              $ Evaluate.ResolveError
              $ Evaluate.FunctionReturnTypeMismatch name targetTyp
              $ HashMap.keys
              $ functionsByType functions

        _ -> Evaluate.throwQueryError $ Evaluate.ResolveError $ Evaluate.UnknownFunction name

    resolveTopLevel :: Text -> Value.Object -> [Evaluate.TopLevel m]
    resolveTopLevel name args =
      [ Evaluate.TopLevel encode
        $ Evaluate.withEvaluate (>>= call)
        $ Decode.JSON.evalFieldDecoder decode args
      | Function _ decode _ (Just encode) call <-
          maybe [] functionsByEncoder $ HashMap.lookup name allFuns
      ]

---

evalQuery
  :: (Applicative m, Decode.HasDecoder a)
  => Evaluate.Resolver m
  -> Value.Value
  -> Either Evaluate.LocatedEvaluateError (m a)
evalQuery resolver =
  evalQueryWith resolver Decode.query

evalQueryWith
  :: Applicative m
  => Evaluate.Resolver m
  -> Decode.Query a
  -> Value.Value
  -> Either Evaluate.LocatedEvaluateError (m a)
evalQueryWith resolver query queryValue =
  Evaluate.runEvaluate resolver $ Decode.JSON.evalQuery query queryValue

---

data TopLevelQueryError
  = TopLevelQueryMustBeCall Value.NoCallValue
  | UnknownTopLevelFunction Text
  | NoFunctionMatched Text Value.Object [TopLevelMatchError]
  | MultipleFunctionsMatched Text Value.Object

data TopLevelMatchError
  = ProjectionError Projection.LocatedProjectionError
  | EvaluateError Evaluate.LocatedEvaluateError

data TopLevelQuery = TopLevelQuery
  { topLevelQuery_query  :: Value.CallValue
  , topLevelQuery_return :: Maybe Schema.Shape
  }

instance Aeson.FromJSON TopLevelQuery where
  parseJSON = Aeson.withObject "TopLevelQuery" $ \object -> do
    return <- object Aeson..:? "return"
    TopLevelQuery
      <$> object Aeson..: "query"
      <*> traverse Decode.JSON.parseJson return

runTopLevelQuery
  :: (Monad m, Reflection.Typeable m)
  => Evaluate.Resolver m
  -> TopLevelQuery
  -> Either TopLevelQueryError (m Aeson.Value)
runTopLevelQuery resolver (TopLevelQuery (Value.CallValue name args) shape) =
  case Evaluate.resolveTopLevel resolver name args of
    [] -> Left $ UnknownTopLevelFunction name
    xs -> do
      let
        match (Evaluate.TopLevel encode eval) = do
          encode <- first ProjectionError $
            maybe (Right encode) (`Projection.projectEncoder` encode) shape
          action <- first EvaluateError $ Evaluate.runEvaluate resolver eval
          Right $ fmap (Encode.JSON.runEncoder encode) action

        matches = map match xs

      case rights matches of
        []     -> Left $ NoFunctionMatched name args $ lefts matches
        [call] -> Right call
        _calls -> Left $ MultipleFunctionsMatched name args
