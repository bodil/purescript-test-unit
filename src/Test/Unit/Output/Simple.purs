module Test.Unit.Output.Simple
  ( runTest
  ) where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Free (runFreeM)
import Data.Either (Either(Left, Right))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Sequence (uncons, length)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (collectErrors, TestF(..), TestSuite, Group(..))

indent :: Int -> String
indent 0 = mempty
indent n = "  " <> indent (n - 1)

printLive :: forall e. TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit
printLive t = runSuite 0 t
  where
    runSuite :: forall e1. Int -> TestSuite (console :: CONSOLE | e1) -> Aff (console :: CONSOLE | e1) Unit
    runSuite level suite = runFreeM (runSuiteItem level) suite

    runSuiteItem :: forall e1. Int -> TestF (console :: CONSOLE | e1) (TestSuite (console :: CONSOLE | e1)) -> Aff (console :: CONSOLE | e1) (TestSuite (console :: CONSOLE | e1))
    runSuiteItem level (TestGroup (Group label content) rest) = do
      log $ indent level ++ "- Suite: " ++ label
      runSuite (level + 1) content
      return rest
    runSuiteItem level t'@(TestUnit label t rest) = do
      result <- attempt t
      case result of
        (Right _) -> log $ indent level ++ "\x2713 Passed: " ++ label
        (Left err) ->
          log $ indent level ++ "\x2620 Failed: " ++ label
                             ++ " because " ++ message err
      return rest

printErrors :: forall e. TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit
printErrors suite = do
  errors <- collectErrors suite
  log ""
  case length errors of
    0 -> log "🎉 All tests passed! 🎉\n"
    1 -> do
      log "1 test failed:\n"
      list errors
    i -> do
      log $ show i ++ " tests failed:\n"
      list errors
  where list = traverse_ print
        print (Tuple path err) = do
          printHeader 0 path
          printError err
          log ""
        printHeader level path = case uncons path of
          Nothing -> return unit
          Just (Tuple car cdr) -> do
            log $ indent level ++ "In \"" ++ car ++ "\":"
            printHeader (level + 1) cdr
        printError err = log $ "Error: " ++ message err

runTest :: forall e. TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit
runTest suite = do
  printLive suite
  printErrors suite
