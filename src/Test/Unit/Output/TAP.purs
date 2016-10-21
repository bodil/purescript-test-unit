module Test.Unit.Output.TAP
  ( runTest
  , requested
  ) where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, message, stack)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldl, sequence_)
import Data.List (toUnfoldable, snoc, length, List(Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), joinWith, split)
import Data.Tuple (snd, Tuple(Tuple))
import Test.Unit (TestList, collectTests, TestSuite)

foreign import requested :: Boolean

printStack :: forall e. Error -> Aff (console :: CONSOLE | e) Unit
printStack err = case stack err of
  Nothing -> pure unit
  Just s -> do
    log $ "  stack: |-"
    log $ joinWith "\n" (append "    " <$> split (Pattern "\n") s)

runTest :: forall e. TestSuite (console :: CONSOLE, avar :: AVAR | e) -> Aff (console :: CONSOLE, avar :: AVAR | e) (TestList (console :: CONSOLE, avar :: AVAR | e))
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
