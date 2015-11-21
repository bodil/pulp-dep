module Pulp.FFI
  ( Promise()
  , runPromise
  , unsafeInspect
  ) where

import Prelude

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())
import Data.Function

foreign import data Promise :: * -> *

foreign import runPromise' :: forall a e. Fn3 (Error -> Eff e Unit) (a -> Eff e Unit) (Promise a) (Eff e Unit)

runPromise :: forall a e. Promise a -> Aff e a
runPromise p = makeAff (\err win -> runFn3 runPromise' err win p)

-- | This is quite unsafe but often useful.
foreign import unsafeInspect :: forall a. a -> String
