module Pulp.Manifest where

import Prelude

import Data.Foreign (Foreign(), F(), unsafeFromForeign, readString)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.StrMap (StrMap())
import Data.Version (Version(), parseVersion)

import Pulp.Foreign as F

type Manifest = Foreign

readManifest :: Foreign -> F Manifest
readManifest f = do
  prop "name" f
  prop "version" f
  return $ unsafeFromForeign f

name :: Manifest -> F String
name = readProp "name"

description :: Manifest -> F String
description = readProp "description"

version :: Manifest -> F Version
version p = readProp "version" p >>= parseVersion >>> F.parseToForeign

dependencies :: Manifest -> F (StrMap String)
dependencies p = prop "dependencies" p >>= F.readStrMap readString

devDependencies :: Manifest -> F (StrMap String)
devDependencies p = prop "devDependencies" p >>= F.readStrMap readString

peerDependencies :: Manifest -> F (StrMap String)
peerDependencies p = prop "peerDependencies" p >>= F.readStrMap readString
