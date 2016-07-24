module Test.Unit.Output.Fancy
  ( runTest
  ) where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Free (runFreeM)
import Data.Either (Either(Left, Right))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mempty)
import Data.List (uncons, length)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (collectErrors, TestF(..), TestSuite, Group(..))
import Test.Unit.Console (printFail, savePos, restorePos, eraseLine, printPass, printLabel, print, TESTOUTPUT)

indent :: Int -> String
indent 0 = mempty
indent n = "  " <> indent (n - 1)

printLive :: forall e. TestSuite (testOutput :: TESTOUTPUT | e) -> Aff (testOutput :: TESTOUTPUT | e) Unit
printLive t = runSuite 0 t
  where
    runSuite level suite = runFreeM (runSuiteItem level) suite

    runSuiteItem level (TestGroup (Group label content) rest) = do
      liftEff do
        print $ indent level
        print "\x2192 Suite: "
        printLabel label
        print "\n"
      runSuite (level + 1) content
      pure rest
    runSuiteItem level t'@(TestUnit label t rest) = do
      liftEff do
        print $ indent level
        savePos
        print "\x2192 Running: "
        printLabel label
        restorePos
      result <- attempt t
      case result of
        (Right _) -> liftEff do
          eraseLine
          printPass "\x2713 Passed: "
          printLabel label
          print "\n"
        (Left err) -> liftEff do
          eraseLine
          printFail "\x2620 Failed: "
          printLabel label
          print " because "
          printFail $ message err
          print "\n"
      pure rest

printErrors :: forall e. TestSuite (testOutput :: TESTOUTPUT | e) -> Aff (testOutput :: TESTOUTPUT | e) Unit
printErrors suite = do
  errors <- collectErrors suite
  liftEff do
    case length errors of
      0 -> pure unit
      1 -> do
        printFail "\n1 test failed:\n\n"
        list errors
      i -> do
        printFail $ "\n" <> show i <> " tests failed:\n\n"
        list errors
  where list = traverse_ printItem
        printItem (Tuple path err) = do
          printHeader 0 path
          printError err
          print "\n"
        printHeader level path = case uncons path of
          Nothing -> print $ indent level
          Just {head, tail} -> do
            print $ indent level <> "In \"" <> head <> "\":\n"
            printHeader (level + 1) tail
        printError err = do
          maybe (printFail $ message err) printFail (stack err)
          print "\n"

runTest :: forall e. TestSuite (testOutput :: TESTOUTPUT | e) -> Aff (testOutput :: TESTOUTPUT | e) Unit
runTest suite = do
  printLive suite
  printErrors suite
