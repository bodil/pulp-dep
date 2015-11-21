module Main where

import Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION(), Error())
import Data.Array ((!!))
import Data.Eulalie.Parser as P
import Data.Eulalie.Result as Res
import Data.Eulalie.Stream as Stream
import Data.Maybe (fromMaybe)
import Node.Process (argv, PROCESS())

import Pulp.Registry as Reg
-- import Pulp.FFI as FFI
import Pulp.Semver.Parser as SP

error :: forall e. Error -> Eff (console :: CONSOLE | e) Unit
error e = Console.log $ show e

ok :: forall a e. a -> Eff e Unit
ok _ = return unit

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, registry :: Reg.REGISTRY, process :: PROCESS | e) Unit
main = Aff.runAff error ok $ do
  args <- liftEff argv
  let arg = fromMaybe "lol" $ args !! 1

  case P.parse SP.range $ Stream.stream arg of
    Res.Success { value } -> do
      log $ show value
      -- log $ show $ Semver.min value
      -- log $ show $ Semver.max value
    Res.Error e -> log $ "FAILED " ++ Data.Eulalie.Error.print e


  -- npm <- Reg.registry "npm"
  -- log $ FFI.unsafeInspect npm
  -- pkg <- Reg.lookup npm "mkdirp"
  -- case Data.StrMap.lookup pkg.latest pkg.versions of
  --   Nothing -> log $ "Latest version reported as " ++ pkg.latest ++ " but absent from version map!"
  --   Just ver -> do
  --     log $ FFI.unsafeInspect ver
  --     log "Downloading..."
  --     r <- Reg.download npm "mkdirp" pkg.latest ver "/home/bodil/Sync/Workspace/lol/deps/mkdirp"
  --     log $ FFI.unsafeInspect r
