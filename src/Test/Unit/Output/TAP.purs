module Test.Unit.Output.TAP
  ( runTest
  , requested
  ) where

import Prelude

import Data.Either (Either(Right, Left))
import Data.Foldable (foldl, sequence_)
import Data.List (toUnfoldable, snoc, length, List(Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), joinWith, split)
import Data.Tuple (snd, Tuple(Tuple))
import Effect.Aff (attempt, Aff)
import Effect.Exception (Error, message, stack)
import Test.Unit (TestList, collectTests, TestSuite)
import Test.Unit.Console (log)

foreign import requested :: Boolean

printStack :: Error -> Aff Unit
printStack err = case stack err of
  Nothing -> pure unit
  Just s -> do
    log $ "  stack: |-"
    log $ joinWith "\n" (append "    " <$> split (Pattern "\n") s)

runTest :: TestSuite -> Aff TestList
runTest suite = do
  tests <- collectTests suite
  log $ "1.." <> show (length tests)
  let acts = foldl run (Tuple 1 Nil) tests
  sequence_ $ snd acts
  pure tests
  where
    run (Tuple count out) (Tuple path test) = Tuple (count+1) $ snoc out do
      let label = joinWith " / " $ toUnfoldable path
      result <- attempt test
      case result of
        (Left err) -> do
          log $ "not ok " <> show count <> " " <> label
          log "  ---"
          log $ "  message: " <> message err
          printStack err
          log "  ..."
        (Right _) -> log $ "ok " <> show count <> " " <> label
