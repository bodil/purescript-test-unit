module Test.Unit.Output.Simple
  ( runTest
  ) where

import Prelude

import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, stack)
import Data.Either (Either(Left, Right))
import Data.Foldable (traverse_)
import Data.List (length, List, uncons)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestList, TestSuite, collectResults, countSkippedTests, keepErrors, walkSuite)

indent :: Int -> String
indent 0 = mempty
indent n = "  " <> indent (n - 1)

indent' :: forall a. List a -> String
indent' = length >>> indent

printLive :: forall e. TestSuite (console :: CONSOLE, avar :: AVAR | e) -> Aff (console :: CONSOLE, avar :: AVAR | e) (TestList (console :: CONSOLE, avar :: AVAR | e))
printLive tst = walkSuite runSuiteItem tst
  where
    runSuiteItem path (Left label) = do
      log $ indent' path <> "- Suite: " <> label
      pure unit
    runSuiteItem path (Right (Tuple label t)) = do
      result <- attempt t
      case result of
        (Right _) -> log $ indent' path <> "\x2713 Passed: " <> label
        (Left err) ->
          log $ indent' path <> "\x2620 Failed: " <> label
                             <> " because " <> message err
      pure unit

printErrors :: forall e. TestList (console :: CONSOLE | e) -> Int -> Aff (console :: CONSOLE | e) Unit
printErrors tests skCount = do
  results <- collectResults tests
  let errors = keepErrors results
      skMsg = case skCount of
          0 -> ""
          1 -> " (1 test skipped)"
          i -> " (" <> show i <> " tests skipped)"
  log ""
  case length errors of
    0 -> log $ "All " <> show (length results) <> " tests passed" <> skMsg <> "!"
    1 -> do
      log $ "1 test failed" <> skMsg <> ":\n"
      list errors
    i -> do
      log $ show i <> " tests failed" <> skMsg <> ":\n"
      list errors
  where list = traverse_ print
        print (Tuple path err) = do
          printHeader 0 path
          printError err
          log ""
        printHeader level path = case uncons path of
          Nothing -> pure unit
          Just {head, tail} -> do
            log $ indent level <> "In \"" <> head <> "\":"
            printHeader (level + 1) tail
        printError err = log $ "Error: " <> fromMaybe (message err) (stack err)

runTest :: forall e. TestSuite (console :: CONSOLE, avar :: AVAR | e) -> Aff (console :: CONSOLE, avar :: AVAR | e) (TestList (console :: CONSOLE, avar :: AVAR | e))
runTest suite = do
  tests <- printLive suite
  printErrors tests (countSkippedTests suite)
  pure tests
