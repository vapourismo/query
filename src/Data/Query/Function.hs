{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Query.Function
  ( Function (..)
  , function
  , functionWith
  , topLevelFunction
  , topLevelFunctionWith
  , mapFunction
  )
where

import qualified Data.Query.Decode as Decode
import qualified Data.Query.Encode as Encode
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Type.Reflection as Reflection

data Function m where
  Function
    :: Text
    -> Decode.FieldsDecoder a
    -> Reflection.TypeRep b
    -> Maybe (Encode.Encoder b)
    -> (a -> m b)
    -> Function m

instance Show (Function m) where
  show = \case
    Function name decode returnType encode _ ->
      Text.unpack name <> " : " <> show decode <> " -> " <> maybe (show returnType) show encode

function
  :: (Decode.HasFieldsDecoder a, Reflection.Typeable b)
  => Text
  -> (a -> m b)
  -> Function m
function name =
  functionWith name Decode.fieldsDecoder

functionWith
  :: Reflection.Typeable b
  => Text
  -> Decode.FieldsDecoder a
  -> (a -> m b)
  -> Function m
functionWith name decode =
  Function name decode Reflection.typeRep Nothing

topLevelFunction
  :: (Decode.HasFieldsDecoder a, Encode.HasEncoder b, Reflection.Typeable b)
  => Text
  -> (a -> m b)
  -> Function m
topLevelFunction name =
  topLevelFunctionWith name Decode.fieldsDecoder Encode.encoder

topLevelFunctionWith
  :: (Encode.HasEncoder b, Reflection.Typeable b)
  => Text
  -> Decode.FieldsDecoder a
  -> Encode.Encoder b
  -> (a -> m b)
  -> Function m
topLevelFunctionWith name decode encode =
  Function name decode Reflection.typeRep (Just encode)

mapFunction :: (forall x. m x -> n x) -> Function m -> Function n
mapFunction f = \case
  Function name decode returnType encode g ->
    Function name decode returnType encode (f . g)
