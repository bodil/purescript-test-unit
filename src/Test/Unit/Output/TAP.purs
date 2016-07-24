module Test.Unit.Output.TAP
  ( runTest
  , requested
  ) where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, message, stack)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldl, sequence_)
import Data.List (toUnfoldable, snoc, length, List(Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (joinWith, split)
import Data.Tuple (snd, Tuple(Tuple))
import Test.Unit (TestSuite, collectTests)

foreign import requested :: Boolean

printStack :: forall e. Error -> Aff (console :: CONSOLE | e) Unit
printStack err = case stack err of
  Nothing -> pure unit
  Just s -> do
    log $ "  stack: |-"
    log $ joinWith "\n" (append "    " <$> split "\n" s)

runTest :: forall e. TestSuite (console :: CONSOLE | e) -> Aff (console :: CONSOLE | e) Unit
runTest suite = do
  let tests = collectTests suite
  log $ "1.." <> show (length tests)
  let acts = foldl run (Tuple 1 Nil) tests
  sequence_ $ snd acts
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
