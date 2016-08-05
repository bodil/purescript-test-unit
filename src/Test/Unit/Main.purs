module Test.Unit.Main
  ( runTest
  , runTestWith
  , run
  , exit
  ) where

import Prelude
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.List (length)
import Test.Unit (collectResults, TestList, keepErrors, TestSuite)
import Test.Unit.Console (hasColours, hasStderr, TESTOUTPUT)

-- | Exit the current process using the provided return code.
-- |
-- | Works on Node and Phantom. Will have no effect on other platforms.
foreign import exit :: forall e. Int -> Eff (console :: CONSOLE | e) Unit

run :: forall e. Aff (console :: CONSOLE | e) Unit -> Eff (console :: CONSOLE | e) Unit
run e = do
  runAff errorHandler successHandler e
  pure unit
  where errorHandler _ = exit 1
        successHandler _ = pure unit

-- | Run a test suite using the provided test runner.
runTestWith  :: forall e. (TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) (TestList (console :: CONSOLE | e))) -> TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit
runTestWith runner suite = do
  results <- runner suite >>= collectResults
  let errs = keepErrors results
  if length errs > 0 then liftEff (exit 1) else pure unit

-- | Run a test suite, picking the most appropriate test runner.
runTest :: forall e. TestSuite (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) -> Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
runTest suite = run $ runTestWith runner suite
  where runner = if TAP.requested
                 then TAP.runTest
                 else if hasStderr && hasColours
                      then Fancy.runTest
                      else Simple.runTest
