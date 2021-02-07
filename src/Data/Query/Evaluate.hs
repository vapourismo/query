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

  , throwQueryError
  , nestQueryError

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
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Encode.Types as Encode
import qualified Data.Query.Types as Types
import qualified Data.Query.Value as Value
import           Data.Text (Text)
import qualified Type.Reflection as Reflection

data ResolveError
  = UnknownFunction Text
  | FunctionReturnTypeMismatch Text Reflection.SomeTypeRep [Reflection.SomeTypeRep]
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
    -> Evaluate m a
    -> TopLevel m

data Resolver m = Resolver
  { resolve         :: forall a. Reflection.TypeRep a -> Text -> Value.Object -> Evaluate m a
  , resolveTopLevel :: Text -> Value.Object -> [TopLevel m]
  }

newtype Evaluate m a = Evaluate
  { unEvaluate :: Compose (Reader.ReaderT (Resolver m) (Either LocatedEvaluateError)) m a }
  deriving newtype (Functor, Applicative)

instance Trans.MonadTrans Evaluate where
  lift = Evaluate . Compose . pure

instance Applicative m => Decode.DecodeContext (Evaluate m) where
  throwDecodeError = throwQueryError . DecodeError

  nestDecodeError = nestQueryError

  callFunction typeRep name args = Evaluate $ Compose $ do
    Resolver resolve _ <- Reader.ask
    getCompose $ unEvaluate $ resolve typeRep name args

runEvaluate :: Resolver m -> Evaluate m a -> Either LocatedEvaluateError (m a)
runEvaluate resolver evaluate =
  Reader.runReaderT (getCompose (unEvaluate evaluate)) resolver

withEvaluate :: (m a -> m b) -> Evaluate m a -> Evaluate m b
withEvaluate f = Evaluate . Compose . fmap f . getCompose . unEvaluate

-- TODO: Rename to throwEvaluateError
throwQueryError :: EvaluateError -> Evaluate m a
throwQueryError = Evaluate . Compose . Except.throwError . LocatedEvaluateError []

-- TODO: Rename to nestEvaluateError
nestQueryError :: Types.Path -> Evaluate m a -> Evaluate m a
nestQueryError path (Evaluate (Compose inner)) =
  Evaluate $ Compose $ Reader.mapReaderT (first addPath) inner
  where
    addPath (LocatedEvaluateError paths error) = LocatedEvaluateError (path : paths) error
