module Test.Main where

import Prelude

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())

import Test.Unit (TestUnit(), TIMER(), test, runTest, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT())

unresolved :: forall e. TestUnit e
unresolved = makeAff \_ _ -> pure unit

main :: forall e. Eff (timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  test "basic asserts" do
    Assert.assert "wasn't true" true
    Assert.assertFalse "wasn't false" false
  test "timeout" do
    Assert.expectFailure "didn't time out" $ timeout 100 unresolved
  test "equal" do
    Assert.equal "omg" "omg"
    Assert.expectFailure "should be unequal" $ Assert.equal "omg" "wat"
