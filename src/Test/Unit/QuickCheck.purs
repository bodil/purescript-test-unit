module Test.Unit.QuickCheck
  ( quickCheck
  , quickCheck'
  ) where

import Prelude

import Control.Monad.Aff (makeAff, liftEff')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM())
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Test.QuickCheck (Testable, Result(..))
import Test.QuickCheck as QC
import Test.QuickCheck.LCG as LCG

import Test.Unit (TestUnit(), success, failure)

quickCheck' :: forall e prop. (Testable prop) => Int -> prop -> TestUnit (random :: RANDOM | e)
quickCheck' tries prop = do
  seed <- liftEff $ LCG.randomSeed
  let results = QC.quickCheckPure seed tries prop
      wins = foldl wins' 0 results
      wins' acc Success = acc + 1
      wins' acc _ = acc
      findErr Nil = Nothing
      findErr (Cons (Failed msg) _) = Just msg
      findErr (Cons _ xs) = findErr xs
  case findErr results of
    Nothing -> success
    Just msg -> failure $ show (tries - wins) ++ "/" ++ show tries ++ " tests failed: " ++ msg

quickCheck :: forall e prop. (Testable prop) => prop -> TestUnit (random :: RANDOM | e)
quickCheck = quickCheck' 100
