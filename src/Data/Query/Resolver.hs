{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Query.Resolver
  ( ResolveError (..)
  , Resolver (..)
  , resolveFunction
  , resolveTopLevelFunction
  )
where

import qualified Data.Aeson as Aeson
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Query.Decode.Types as Decode
import qualified Data.Query.Encode.JSON as Encode
import qualified Data.Query.Encode.Projection as Projection
import qualified Data.Query.Function as Function
import qualified Data.Query.Shape as Shape
import           Data.Text (Text)
import qualified Type.Reflection as Reflection

data ResolveError
  = UnknownFunction
      Text -- ^ Function name
  | FunctionReturnTypeMismatch
      Text -- ^ Function name
      Reflection.SomeTypeRep -- ^ Wanted return type
      [Reflection.SomeTypeRep] -- ^ Available return types
  | forall m. NoMatchingTopLevelFunction
      Text -- ^ Function name
      [(Function.Function m, Projection.LocatedProjectionError)]
  | forall m. MultipleTopLevelFunctionsMatch
      Text -- ^ Function name
      [(Function.Function m, Decode.FieldsDecoder (m Aeson.Value))]

deriving instance Show ResolveError

newtype Resolver m = Resolver
  { resolver_functions
      :: HashMap.HashMap Text (HashMap.HashMap Reflection.SomeTypeRep (Function.Function m))
  }

resolveFunction
  :: Resolver m
  -> Text
  -> Reflection.TypeRep a
  -> Either ResolveError (Decode.FieldsDecoder (m a))
resolveFunction resolver name retType =
  case HashMap.lookup name (resolver_functions resolver) of
    Just functionsByType ->
      case HashMap.lookup (Reflection.SomeTypeRep retType) functionsByType of
        Just (Function.Function _ decode funRetType _)
          | Just Reflection.HRefl <- Reflection.eqTypeRep retType funRetType ->
            Right decode

        _ -> Left $
          FunctionReturnTypeMismatch
            name
            (Reflection.SomeTypeRep retType)
            (HashMap.keys functionsByType)

    _ -> Left $ UnknownFunction name

resolveTopLevelFunction
  :: Functor m
  => Resolver m
  -> Text
  -> Maybe Shape.Shape
  -> Either ResolveError (Decode.FieldsDecoder (m Aeson.Value))
resolveTopLevelFunction resolver name shape =
  case HashMap.lookup name (resolver_functions resolver) of
    Just functionsByType ->
      let
        matches = match (HashMap.elems functionsByType)
      in
        case rights matches of
          [] -> Left $ NoMatchingTopLevelFunction name $ lefts matches
          [(_, decoder)] -> Right decoder
          allMatches -> Left $ MultipleTopLevelFunctionsMatch name allMatches

    Nothing -> Left $ UnknownFunction name
  where
    combine decode encode = fmap (Encode.runEncoder encode) <$> decode

    match funs =
      [ bimap (fun,) (fun,) $
          combine decode <$> maybe (Right encoder) (`Projection.projectEncoder` encoder) shape
      | fun@(Function.Function _ decode _ (Just encoder)) <- funs
      ]
