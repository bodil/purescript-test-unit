module Test.Unit.Assert
  ( assert
  , assertFalse
  , expectFailure
  , equal
  , equal'
  , shouldEqual
  ) where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Either (either)
import Test.Unit (success, failure, Test)

-- | Given a failure reason and a boolean, either succeed if the boolean is
-- | true, or fail if the boolean is false.
assert :: forall e. String -> Boolean -> Test e
assert _ true = success
assert reason false = failure reason

-- | The reverse of `assert`: given a failure reason and a boolean, either
-- | succeed if the boolean is false, or fail if the boolean is true.
assertFalse :: forall e. String -> Boolean -> Test e
assertFalse _ false = success
assertFalse reason true = failure reason

-- | Expect a test to fail. Given a reason and a test, fail with the given
-- | reason if the test succeeds, and succeed if it fails.
expectFailure :: forall e. String -> Test e -> Test e
expectFailure reason t = do
  r <- attempt t
  either (const success) (const $ failure reason) r

-- | Assert that two printable values are equal.
equal :: forall a e. Eq a => Show a => a -> a -> Test e
equal expected actual =
  if expected == actual then success
  else failure $ "expected " <> show expected <>
       ", got " <> show actual

-- | Assert that two non-printable values are equal, using a provided failure
-- | string instead of generating one.
equal' :: forall a e. (Eq a) => String -> a -> a -> Test e
equal' reason expected actual =
  if expected == actual then success else failure reason

-- | Assert that two printable values are equal, like `equal` but also adds
-- | a title string to the error message.
equal'' :: forall a e. Eq a => Show a => String -> a -> a -> Test e
equal'' name a b = catchError (equal a b) (throwError <<< error <<< ((name <> " ") <> _) <<< message)

-- | `shouldEqual` is equivalent to `equal`, with the arguments flipped,
-- | for people who prefer the BDD style.
-- |
-- |     it "should do what I expect of it" do
-- |       result `shouldEqual` "expected result"
shouldEqual :: forall a e. Eq a => Show a => a -> a -> Test e
shouldEqual = flip equal
