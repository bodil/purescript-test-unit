module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Control.Monad.Aff (never)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref as Ref
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, suite, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTestWith, run)
import Test.Unit.QuickCheck (quickCheck)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

tests :: forall e. Ref.Ref Int -> TestSuite (avar :: AVAR , random :: RANDOM, ref :: Ref.REF | e)
tests ref = do
  test "basic asserts" do
    Assert.assert "wasn't true" true
    Assert.assertFalse "wasn't false" false
  test "timeout" do
    Assert.expectFailure "didn't time out" $ timeout 100 never
  test "equal" do
    Assert.equal "omg" "omg"
    Assert.expectFailure "should be unequal" $ Assert.equal "omg" "wat"
  test "quickcheck" do
    quickCheck theCommutativeProperty
  suite "a test suite" do
    test "a test in a test suite" do
      pure unit
  test "tests run only once: part 1" do
    liftEff $ Ref.modifyRef ref (_ + 1)
  test "tests run only once: part deux" do
    Assert.equal 1 =<< liftEff (Ref.readRef ref)

main :: forall e. Eff (avar :: AVAR, console :: CONSOLE, random :: RANDOM, ref :: Ref.REF, testOutput :: TESTOUTPUT | e) Unit
main = run do
  ref <- liftEff $ Ref.newRef 0
  runTestWith Fancy.runTest $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith Simple.runTest $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith TAP.runTest $ tests ref
