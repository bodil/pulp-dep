module Pulp.Semver.Parser
  ( version
  , range
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Eulalie.Char as Char
import Data.Eulalie.Char.Predicates as Pred
import Data.Eulalie.Parser (Parser())
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import Data.Foldable (fold)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String as Str

import Pulp.Semver (Version(..), VersionRange(Gt, GtEq, Lt, LtEq, Eq, Any), intersection, intersections)

nonZero :: Parser String
nonZero = S.fromChar (P.sat $ Pred.unless Pred.isDigit \x -> x == '0')

digit :: Parser String
digit = S.fromChar Char.digit

nr :: Parser String
nr = S.char '0' <|> nonZero <> S.many digit

xr :: Parser String
xr = S.char '*' <|> S.char 'X' <|> S.char 'x'

part :: Parser String
part = nr <|> S.many1 (S.char '-' <|> S.fromChar Char.alphanum)

parts :: Parser String
parts = part <> S.many (S.char '.' <> part)

qualifier :: Parser (Maybe String)
qualifier = do
  q <- P.maybe (S.char '-' <> parts) <> P.maybe (S.char '+' <> parts)
  return $ if Str.null q then Nothing else Just q

version :: Parser Version
version = do
    major <- S.int
    S.char '.'
    minor <- S.int
    S.char '.'
    patch <- S.int
    qual <- qualifier
    return $ Version major minor patch qual
  <|> do
    major <- S.int
    S.char '.'
    minor <- S.int
    return $ Version major minor 0 Nothing
  <|> do
    major <- S.int
    return $ Version major 0 0 Nothing

partial :: Parser VersionRange
partial = do
  (xr *> return Any)
  <|> do
    major <- S.int
    S.char '.'
    xr
    return $ intersection (GtEq (Version major 0 0 Nothing))
      (Lt (Version (major+1) 0 0 Nothing))
  <|> do
    major <- S.int
    S.char '.'
    minor <- S.int
    S.char '.'
    xr
    return $ intersection (GtEq (Version major minor 0 Nothing))
      (Lt (Version major (minor+1) 0 Nothing))
  <|> do
    major <- S.int
    S.char '.'
    minor <- S.int
    S.char '.'
    patch <- S.int
    qual <- qualifier
    return $ Eq (Version major minor patch qual)
  <|> do
    major <- S.int
    S.char '.'
    minor <- S.int
    return $ intersection (GtEq (Version major minor 0 Nothing))
      (Lt (Version major (minor+1) 0 Nothing))
  <|> do
    major <- S.int
    return $ intersection (GtEq (Version major 0 0 Nothing))
      (Lt (Version (major+1) 0 0 Nothing))

hyphen :: Parser VersionRange
hyphen = do
  v1 <- version
  S.string " - "
  v2 <- version
  return $ intersection (GtEq v1) (LtEq v2)

exactly :: Parser VersionRange
exactly = do
  S.char '='
  v <- version
  return $ Eq v

lessThanOrEqual :: Parser VersionRange
lessThanOrEqual = do
  S.string "<="
  v <- version
  return $ LtEq v

lessThan :: Parser VersionRange
lessThan = do
  S.string "<"
  v <- version
  return $ Lt v

greaterThanOrEqual :: Parser VersionRange
greaterThanOrEqual = do
  S.string ">="
  v <- version
  return $ GtEq v

greaterThan :: Parser VersionRange
greaterThan = do
  S.string ">"
  v <- version
  return $ Gt v

comparator :: Parser VersionRange
comparator = exactly <|> lessThanOrEqual <|> lessThan
             <|> greaterThanOrEqual <|> greaterThan

singleRange :: Parser VersionRange
singleRange = do
  r <- hyphen <|> partial <|> comparator
  return r

and :: Parser VersionRange
and = do
  v <- singleRange
  vs <- P.many1 do
    S.spaces1
    singleRange
  return $ intersections (Cons v vs)

or :: Parser VersionRange
or = do
  v <- and <|> singleRange
  vs <- P.many1 do
    S.string " || "
    and <|> singleRange
  return $ fold (Cons v vs)

range :: Parser VersionRange
range = do
  r <- or <|> and <|> singleRange
  P.eof
  return r
