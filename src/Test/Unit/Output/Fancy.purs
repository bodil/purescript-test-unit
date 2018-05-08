module Test.Unit.Output.Fancy
  ( runTest
  ) where

import Prelude

import Effect.Aff (attempt, Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message, stack)
import Data.Either (Either(Left, Right))
import Data.Foldable (traverse_)
import Data.List (List, uncons, length)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestList, TestSuite, collectResults, countSkippedTests, keepErrors, walkSuite)
import Test.Unit.Console (printFail, savePos, restorePos, eraseLine, printPass, printLabel, print)

indent :: Int -> String
indent 0 = mempty
indent n = "  " <> indent (n - 1)

indent' :: forall a. List a -> String
indent' = length >>> indent

printLive :: TestSuite -> Aff TestList
printLive tst = walkSuite runSuiteItem tst
  where
    runSuiteItem path (Left label) = do
      liftEffect do
        print $ indent' path
        print "\x2192 Suite: "
        printLabel label
        void $ print "\n"
    runSuiteItem path (Right (Tuple label t)) = do
      liftEffect do
        print $ indent' path
        savePos
        print "\x2192 Running: "
        printLabel label
        restorePos
      result <- attempt t
      void $ case result of
        (Right _) -> liftEffect do
          eraseLine
          printPass "\x2713 Passed: "
          printLabel label
          print "\n"
        (Left err) -> liftEffect do
          eraseLine
          printFail "\x2620 Failed: "
          printLabel label
          print " because "
          printFail $ message err
          print "\n"


printErrors :: TestList -> Int -> Aff Unit
printErrors tests skCount = do
  results <- collectResults tests
  let errors = keepErrors results
      skMsg = case skCount of
          0 -> ""
          1 -> " (1 test skipped)"
          i -> " (" <> show i <> " tests skipped)"
  liftEffect do
    case length errors of
      0 -> printPass $ "\nAll " <> show (length results) <> " tests passed" <> skMsg <> "! 🎉\n"
      1 -> do
        printFail $ "\n1 test failed" <> skMsg <>":\n\n"
        list errors
      i -> do
        printFail $ "\n" <> show i <> " tests failed" <> skMsg <> ":\n\n"
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

runTest :: TestSuite -> Aff TestList
runTest suite = do
  tests <- printLive suite
  printErrors tests (countSkippedTests suite)
  pure tests
