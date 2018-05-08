module Test.Unit.Assert
  ( assert
  , assertFalse
  , expectFailure
  , equal
  , equal'
  , shouldEqual
  ) where

import Prelude

import Effect.Aff (attempt)
import Effect.Exception (error, message)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Either (either)
import Test.Unit (success, failure, Test)

-- | Given a failure reason and a boolean, either succeed if the boolean is
-- | true, or fail if the boolean is false.
assert :: String -> Boolean -> Test
assert _ true = success
assert reason false = failure reason

-- | The reverse of `assert`: given a failure reason and a boolean, either
-- | succeed if the boolean is false, or fail if the boolean is true.
assertFalse :: String -> Boolean -> Test
assertFalse _ false = success
assertFalse reason true = failure reason

-- | Expect a test to fail. Given a reason and a test, fail with the given
-- | reason if the test succeeds, and succeed if it fails.
expectFailure :: String -> Test -> Test
expectFailure reason t = do
  r <- attempt t
  either (const success) (const $ failure reason) r

-- | Assert that two printable values are equal.
equal :: forall a. Eq a => Show a => a -> a -> Test
equal expected actual =
  if expected == actual then success
  else failure $ "expected " <> show expected <>
       ", got " <> show actual

-- | Assert that two non-printable values are equal, using a provided failure
-- | string instead of generating one.
equal' :: forall a. (Eq a) => String -> a -> a -> Test
equal' reason expected actual =
  if expected == actual then success else failure reason

-- | Assert that two printable values are equal, like `equal` but also adds
-- | a title string to the error message.
equal'' :: forall a. Eq a => Show a => String -> a -> a -> Test
equal'' name a b = catchError (equal a b) (throwError <<< error <<< ((name <> " ") <> _) <<< message)

-- | `shouldEqual` is equivalent to `equal`, with the arguments flipped,
-- | for people who prefer the BDD style.
-- |
-- |     it "should do what I expect of it" do
-- |       result `shouldEqual` "expected result"
shouldEqual :: forall a. Eq a => Show a => a -> a -> Test
shouldEqual = flip equal
