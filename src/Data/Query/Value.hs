{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Query.Value
  ( Value (..)
  , CallValue (..)
  , NoCallValue (..)
  , toNoCallValue
  , Object
  , Array
  )
where

import qualified Data.Aeson as Aeson
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector

type Object = HashMap.HashMap Text Value

type Array = Vector.Vector Value

data NoCallValue
  = Object Object
  | Array Array
  | Number Scientific
  | String Text
  | Bool Bool
  | Null
  deriving Show

instance Aeson.FromJSON NoCallValue where
  parseJSON = \case
    Aeson.Object object -> Object <$> traverse Aeson.parseJSON object
    Aeson.Array array -> Array <$> traverse Aeson.parseJSON array
    Aeson.Number number -> pure $ Number number
    Aeson.String string -> pure $ String string
    Aeson.Bool bool -> pure $ Bool bool
    Aeson.Null -> pure Null

toNoCallValue :: Aeson.Value -> NoCallValue
toNoCallValue = \case
  Aeson.Object object -> Object $ HashMap.map (NoCall . toNoCallValue) object
  Aeson.Array array -> Array $ Vector.map (NoCall . toNoCallValue) array
  Aeson.Number number -> Number number
  Aeson.String string -> String string
  Aeson.Bool bool -> Bool bool
  Aeson.Null -> Null

data CallValue = CallValue Text Object
  deriving Show

instance Aeson.FromJSON CallValue where
  parseJSON = Aeson.withObject "CallValue" $ \object ->
    CallValue
      <$> object Aeson..: "@"
      <*> traverse Aeson.parseJSON (HashMap.delete "@" object)

data Value
  = NoCall NoCallValue
  | Call CallValue
  deriving Show

instance Aeson.FromJSON Value where
  parseJSON value = asum
      [ Call <$> Aeson.parseJSON value
      , NoCall <$> Aeson.parseJSON value
      ]
