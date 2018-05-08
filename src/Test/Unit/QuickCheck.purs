module Test.Unit.QuickCheck
  ( quickCheck
  , quickCheck'
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Random.LCG (randomSeed)
import Test.QuickCheck (class Testable, Result(..), quickCheckPure)
import Test.Unit (Test, success, failure)

quickCheck' :: forall prop. (Testable prop) => Int -> prop -> Test
quickCheck' tries prop = do
  seed <- liftEffect $ randomSeed
  let results = quickCheckPure seed tries prop
      wins = foldl wins' 0 results
      wins' acc Success = acc + 1
      wins' acc _ = acc
      findErr Nil = Nothing
      findErr (Cons (Failed msg) _) = Just msg
      findErr (Cons _ xs) = findErr xs
  case findErr results of
    Nothing -> success
    Just msg -> failure $ show (tries - wins) <> "/" <> show tries <> " tests failed: " <> msg

quickCheck :: forall prop. (Testable prop) => prop -> Test
quickCheck = quickCheck' 100
