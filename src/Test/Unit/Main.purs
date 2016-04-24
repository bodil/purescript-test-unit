module Test.Unit.Main
  ( runTest
  , runTestWith
  , run
  , exit
  ) where

import Prelude
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Sequence (length)
import Test.Unit (collectErrors, TestSuite)
import Test.Unit.Console (hasColours, hasStderr, TESTOUTPUT)

-- | Exit the current process using the provided return code.
-- |
-- | Works on Node and Phantom. Will have no effect on other platforms.
foreign import exit :: forall e. Int -> Eff (console :: CONSOLE | e) Unit

run :: forall e. Aff (console :: CONSOLE | e) Unit -> Eff (console :: CONSOLE | e) Unit
run = runAff errorHandler successHandler
  where errorHandler _ = exit 1
        successHandler _ = return unit

-- | Run a test suite using the provided test runner.
runTestWith  :: forall e. (TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit) -> TestSuite (console :: CONSOLE | e) -> Eff (console :: CONSOLE | e) Unit
runTestWith runner suite = run $ do
  runner suite
  errs <- collectErrors suite
  if length errs > 0 then liftEff (exit 1) else return unit

-- | Run a test suite, picking the most appropriate test runner.
runTest :: forall e. TestSuite (console :: CONSOLE, testOutput :: TESTOUTPUT | e) -> Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
runTest suite = runTestWith runner suite
  where runner = if Test.Unit.Output.TAP.requested
                 then Test.Unit.Output.TAP.runTest
                 else if hasStderr && hasColours
                      then Test.Unit.Output.Fancy.runTest
                      else Test.Unit.Output.Simple.runTest
