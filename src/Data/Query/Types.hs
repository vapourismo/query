{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Query.Types where

import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Generics.SOP as Generics

-- | Path into an encoded 'Data.Query.Value.Value', 'Schema', 'Data.Query.Decode.Types.Decoder'
-- or 'Data.Query.Encode.Types.Encoder'
data Path
  = ArrayPath
  -- ^ Items of an array
  | StringMapPath
  -- ^ Mapping value of a string map
  | ConstructorPath Text
  -- ^ Variant constructor body
  | FieldPath Text
  -- ^ Record field
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Generics.Generic, Generics.HasDatatypeInfo)
