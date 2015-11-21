module Pulp.Registry
  ( REGISTRY()
  , Registry()
  , registry
  , lookup
  , download
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Data.Foreign (Foreign(), F())
import Data.Foreign.Index (prop)
import Data.Foreign.Class (readProp)
import Data.StrMap (StrMap())

import Pulp.FFI (Promise(), runPromise)
import Pulp.Manifest (Manifest(), readManifest)
import Pulp.Foreign as F

foreign import data Registry :: *
foreign import data REGISTRY :: !

foreign import registry' :: String -> Promise Registry

registry :: forall e. String -> Aff (registry :: REGISTRY | e) Registry
registry r = runPromise $ registry' r



type PackageVersion = { hash :: String, stable :: Boolean, meta :: Manifest }
type LookupResult = { versions :: StrMap PackageVersion, latest :: String }

readPkgVer :: Foreign -> F PackageVersion
readPkgVer f = do
  hash <- readProp "hash" f
  stable <- readProp "stable" f
  meta <- prop "meta" f >>= readManifest
  return { hash, stable, meta }

foreignToLookupResult :: Foreign -> F LookupResult
foreignToLookupResult f = do
  versionsF <- prop "versions" f
  versions <- F.readStrMap readPkgVer versionsF
  latest <- readProp "latest" f
  return { latest, versions }

foreign import lookup' :: Registry -> String -> Promise Foreign

lookup :: forall e. Registry -> String -> Aff (registry :: REGISTRY | e) LookupResult
lookup r pkg = do
  r <- runPromise $ lookup' r pkg
  return $ Data.Either.Unsafe.fromRight $ foreignToLookupResult r



foreign import download' :: Registry -> String -> String -> PackageVersion -> String -> Promise Manifest

download :: forall e. Registry -> String -> String -> PackageVersion -> String -> Aff e Manifest
download r pkg ver entry path = do
  r <- runPromise $ download' r pkg ver entry path
  return r
