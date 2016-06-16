module Test.Unit.QuickCheck
  ( quickCheck
  , quickCheck'
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM())
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Test.QuickCheck (class Testable, Result(..), quickCheckPure)
import Test.QuickCheck.LCG (randomSeed)
import Test.Unit (Test, success, failure)

quickCheck' :: forall e prop. (Testable prop) => Int -> prop -> Test (random :: RANDOM | e)
quickCheck' tries prop = do
  seed <- liftEff $ randomSeed
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

quickCheck :: forall e prop. (Testable prop) => prop -> Test (random :: RANDOM | e)
quickCheck = quickCheck' 100
