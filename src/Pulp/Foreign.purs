module Pulp.Foreign where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (Foldable)
import Data.Foreign (Foreign(), F(), ForeignError(..))
import Data.Foreign.Index (prop)
import Data.List as List
import Data.StrMap (StrMap())
import Data.StrMap as StrMap
import Data.Traversable as Traversable
import Text.Parsing.Parser (ParseError())

parseToForeign :: forall a. Either ParseError a -> F a
parseToForeign p = either (Left <<< JSONError <<< show) Right p

zipMap :: forall a f g. (Foldable f, Foldable g) => f String -> g a -> StrMap a
zipMap a b = StrMap.fromList $ List.zip (List.toList a) (List.toList b)

readStrMap :: forall a. (Foreign -> F a) -> Foreign -> F (StrMap a)
readStrMap convert f = do
  keys <- Data.Foreign.Keys.keys f
  vals <- Traversable.sequence $ flip bind convert <$> flip prop f <$> keys
  return $ zipMap keys vals
