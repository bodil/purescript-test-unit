module Test.Unit.Main
  ( runTest
  , runTestWith
  , run
  , exit
  ) where

import Prelude

import Effect.Aff (runAff, Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Either (either)
import Data.List (length)
import Test.Unit (TestList, TestSuite, collectResults, filterTests, keepErrors)
import Test.Unit.Console (hasColours, hasStderr)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP

-- | Exit the current process using the provided return code.
-- |
-- | Works on Node and Phantom. Will have no effect on other platforms.
foreign import exit :: Int -> Effect Unit

run :: Aff Unit -> Effect Unit
run e = do
  _ <- runAff (either errorHandler successHandler) e
  pure unit
  where errorHandler _ = exit 1
        successHandler _ = pure unit

-- | Run a test suite using the provided test runner.
runTestWith :: (TestSuite -> Aff TestList) -> TestSuite -> Aff Unit
runTestWith runner suite = do
  results <- runner (filterTests suite) >>= collectResults
  let errs = keepErrors results
  if length errs > 0 then liftEffect (exit 1) else pure unit

-- | Run a test suite, picking the most appropriate test runner.
runTest :: TestSuite -> Effect Unit
runTest suite = run $ runTestWith runner suite
  where runner = if TAP.requested
                 then TAP.runTest
                 else if hasStderr && hasColours
                      then Fancy.runTest
                      else Simple.runTest
