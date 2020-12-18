{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Options for the generation of 'Schema.Schema', 'Encode.Encoder' and 'Decode.Decoder' via
-- generics.
module Data.Query.Generic.Options
  ( Options (..)
  , defaultOptions

    -- * Means to demote type-level options to 'Options'
  , KnownGenericOption (..)
  , demoteOptions

    -- * Known type-level options
  , CamelCaseFields
  , TrimFieldTillUnderscore
  , TrimFieldPrefix
  , TrimVariantConstructorPrefix
  , TrimEnumItemPrefix
  )
where

import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Endo (..))
import qualified Data.SOP as SOP
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

stripPrefix :: Text -> Text -> Text
stripPrefix prefix name =
  fromMaybe name $ Text.stripPrefix prefix name

-- | Options for the generation of 'Schema.Schema', 'Encode.Encoder' and 'Decode.Decoder' via
-- generics.
data Options = Options
  { fieldNameModifier          :: Text -> Text
    -- ^ Modifier for the names of fields in records
  , enumItemModifier           :: Text -> Text
    -- ^ Modifier for the names of items of an enumeration
  , variantConstructorModifier :: Text -> Text
    -- ^ Modifier for the names of variant constructors
  }

-- | Default generation options
defaultOptions :: Options
defaultOptions = Options
  { fieldNameModifier = id
  , enumItemModifier = id
  , variantConstructorModifier = id
  }

-- | @option@ can be demoted to 'Endo' 'Options'.
class KnownGenericOption (option :: k) where
  genericOption :: SOP.K (Endo Options) option

-- | Make sure the first letter of field names are lowercase
data CamelCaseFields

instance KnownGenericOption CamelCaseFields where
  genericOption = SOP.K $ Endo $ \options -> options
    { fieldNameModifier = fieldNameModifier options . \name ->
        maybe name (\(h, t) -> Text.cons (toLower h) t) $ Text.uncons name
    }

-- | Remove everything left of and including the @_@ in field names
data TrimFieldTillUnderscore

instance KnownGenericOption TrimFieldTillUnderscore where
  genericOption = SOP.K $ Endo $ \options -> options
    { fieldNameModifier = fieldNameModifier options . \name ->
        case Text.break (== '_') name of
          (_, Text.uncons -> Just (_, name)) -> name
          _                                  -> name
    }

-- | Remove a given prefix from field names
data TrimFieldPrefix (prefix :: Symbol)

instance KnownSymbol prefix => KnownGenericOption (TrimFieldPrefix prefix) where
  genericOption = SOP.K $ Endo $ \options -> options
    { fieldNameModifier = fieldNameModifier options . \name ->
        stripPrefix (Text.pack (symbolVal @prefix SOP.Proxy)) name
    }

-- | Remove a given prefix from variant constructor names
data TrimVariantConstructorPrefix (prefix :: Symbol)

instance KnownSymbol prefix => KnownGenericOption (TrimVariantConstructorPrefix prefix) where
  genericOption = SOP.K $ Endo $ \options -> options
    { variantConstructorModifier = variantConstructorModifier options . \name ->
        stripPrefix (Text.pack (symbolVal @prefix SOP.Proxy)) name
    }

-- | Remove a given prefix from enum item names
data TrimEnumItemPrefix (prefix :: Symbol)

instance KnownSymbol prefix => KnownGenericOption (TrimEnumItemPrefix prefix) where
  genericOption = SOP.K $ Endo $ \options -> options
    { enumItemModifier = enumItemModifier options . \name ->
        stripPrefix (Text.pack (symbolVal @prefix SOP.Proxy)) name
    }

instantiateGenericOptions :: SOP.All KnownGenericOption xs => SOP.NP (SOP.K (Endo Options)) xs
instantiateGenericOptions = SOP.hcpure (SOP.Proxy :: SOP.Proxy KnownGenericOption) genericOption

foldOptions :: SOP.SListI xs => SOP.NP (SOP.K (Endo Options)) xs -> Options
foldOptions options =
  appEndo (mconcat (SOP.hcollapse options)) defaultOptions

-- | Turn a list of type-level options into 'Options'.
demoteOptions
  :: forall options proxy
  .  SOP.All KnownGenericOption options
  => proxy options
  -> Options
demoteOptions _ =
  foldOptions @options instantiateGenericOptions
