{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Query.Evaluate
  ( Evaluate
  , runEvaluate
  , withEvaluate

  , withFunction
  , withTopLevelFunctions

  , throwEvaluateError
  , nestEvaluateError

  , TopLevel (..)
  , Resolver (..)

  , LocatedEvaluateError (..)
  , EvaluateError (..)
  , ResolveError (..)
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import           Data.Bifunctor (Bifunctor (first))
import           Data.Functor.Compose (Compose (Compose), getCompose)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Encode.Types as Encode
import qualified Data.Query.Function as Function
import qualified Data.Query.Types as Types
import           Data.Text (Text)
import qualified Type.Reflection as Reflection

data ResolveError
  = UnknownFunction Text
  | FunctionReturnTypeMismatch Text Reflection.SomeTypeRep [Reflection.SomeTypeRep]
  | UnknownTopLevelFunction Text
  deriving Show

data EvaluateError
  = DecodeError Decode.DecodeError
  | ResolveError ResolveError
  deriving Show

data LocatedEvaluateError = LocatedEvaluateError [Types.Path] EvaluateError
  deriving Show

data TopLevel m where
  TopLevel
    :: Encode.Encoder a
    -> Decode.FieldsDecoder (m a)
    -> TopLevel m

newtype Resolver m = Resolver
  { resolver_functions
      :: HashMap.HashMap Text (HashMap.HashMap Reflection.SomeTypeRep (Function.Function m))
  }

withFunction
  :: Text
  -> Reflection.TypeRep a
  -> (Decode.FieldsDecoder (m a) -> Evaluate m b)
  -> Evaluate m b
withFunction name retType continue = withResolver $ \resolver -> do
  case HashMap.lookup name (resolver_functions resolver) of
    Just functionsByType ->
      case HashMap.lookup (Reflection.SomeTypeRep retType) functionsByType of
        Just (Function.Function _ decode funRetType _)
          | Just Reflection.HRefl <- Reflection.eqTypeRep retType funRetType ->
            continue decode

        _ ->
          throwEvaluateError $ ResolveError $
            FunctionReturnTypeMismatch
              name
              (Reflection.SomeTypeRep retType)
              (HashMap.keys functionsByType)

    _ -> throwEvaluateError $ ResolveError $ UnknownFunction name

withTopLevelFunctions
  :: Text
  -> (NonEmpty (TopLevel m) -> Evaluate m b)
  -> Evaluate m b
withTopLevelFunctions name continue = withResolver $ \resolver -> do
  case HashMap.lookup name (resolver_functions resolver) of
    Just functionsByType ->
      let
        selectedFunctions =
          [ TopLevel encode decode
          | Function.Function _ decode  _ (Just encode) <- HashMap.elems functionsByType
          ]
      in
        case selectedFunctions of
          t : ts -> continue $ t :| ts
          [] -> throwEvaluateError $ ResolveError $ UnknownTopLevelFunction name

    _ -> throwEvaluateError $ ResolveError $ UnknownFunction name

newtype Evaluate m a = Evaluate
  { unEvaluate :: Compose (Reader.ReaderT (Resolver m) (Either LocatedEvaluateError)) m a }
  deriving newtype (Functor, Applicative)

instance Trans.MonadTrans Evaluate where
  lift = Evaluate . Compose . pure

runEvaluate :: Resolver m -> Evaluate m a -> Either LocatedEvaluateError (m a)
runEvaluate resolver evaluate =
  Reader.runReaderT (getCompose (unEvaluate evaluate)) resolver

withEvaluate :: (m a -> m b) -> Evaluate m a -> Evaluate m b
withEvaluate f = Evaluate . Compose . fmap f . getCompose . unEvaluate

withResolver :: (Resolver m -> Evaluate m a) -> Evaluate m a
withResolver f = Evaluate $ Compose $ do
  resolver <- Reader.ask
  getCompose $ unEvaluate $ f resolver

throwEvaluateError :: EvaluateError -> Evaluate m a
throwEvaluateError = Evaluate . Compose . Except.throwError . LocatedEvaluateError []

nestEvaluateError :: Types.Path -> Evaluate m a -> Evaluate m a
nestEvaluateError path (Evaluate (Compose inner)) =
  Evaluate $ Compose $ Reader.mapReaderT (first addPath) inner
  where
    addPath (LocatedEvaluateError paths error) = LocatedEvaluateError (path : paths) error
