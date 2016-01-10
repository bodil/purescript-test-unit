module Test.Unit.Assert
  ( assert
  , assertFalse
  , expectFailure
  , equal
  ) where

import Prelude
import Data.Either (either)
import Control.Monad.Aff (attempt)
import Test.Unit (success, failure, Assertion(), TestUnit())

-- | Given a failure reason and a boolean, either succeed if the boolean is
-- | true, or fail if the boolean is false.
assert :: forall e. String -> Boolean -> Assertion e
assert _ true = success
assert reason false = failure reason

-- | The reverse of `assert`: given a failure reason and a boolean, either
-- | succeed if the boolean is false, or fail if the boolean is true.
assertFalse :: forall e. String -> Boolean -> Assertion e
assertFalse _ false = success
assertFalse reason true = failure reason

-- | Expect a test to fail. Given a reason and a test, fail with the given
-- | reason if the test succeeds, and succeed if it fails.
expectFailure :: forall e. String -> TestUnit e -> TestUnit e
expectFailure reason t = do
  r <- attempt t
  either (const success) (const $ failure reason) r

-- | Assert that two printable values are equal.
equal :: forall a e. (Eq a, Show a) => a -> a -> Assertion e
equal expected actual =
  if expected == actual then success
  else failure $ "expected " ++ show expected ++
       ", got " ++ show actual

-- | Assert that two non-printable values are equal, using a provided failure
-- | string instead of generating one.
equal' :: forall a e. (Eq a) => String -> a -> a -> Assertion e
equal' reason expected actual =
  if expected == actual then success else failure reason
