module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, suite, Test, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTestWith, run)
import Test.Unit.QuickCheck (quickCheck)

unresolved :: forall e. Test e
unresolved = makeAff \_ _ -> pure unit

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

foreign import incRef :: forall e. Aff e Int
foreign import resetRef :: forall e. Aff e Int

tests :: forall e. TestSuite (timer :: TIMER , avar :: AVAR , random :: RANDOM | e)
tests = do
  test "basic asserts" do
    Assert.assert "wasn't true" true
    Assert.assertFalse "wasn't false" false
  test "timeout" do
    Assert.expectFailure "didn't time out" $ timeout 100 unresolved
  test "equal" do
    Assert.equal "omg" "omg"
    Assert.expectFailure "should be unequal" $ Assert.equal "omg" "wat"
  test "quickcheck" do
    quickCheck theCommutativeProperty
  suite "a test suite" do
    test "a test in a test suite" do
      pure unit
  test "tests run only once" do
    ref <- incRef
    Assert.equal 1 ref

main :: forall e. Eff (timer :: TIMER, avar :: AVAR, console :: CONSOLE, random :: RANDOM, testOutput :: TESTOUTPUT | e) Unit
main = run do
  resetRef
  runTestWith Fancy.runTest tests
  resetRef
  runTestWith Simple.runTest tests
  resetRef
  runTestWith TAP.runTest tests
