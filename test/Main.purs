module Test.Main where

import Prelude
import Control.Monad.Aff (never)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref as Ref
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, failure, suite, suiteOnly, suiteSkip, test, testOnly, testSkip, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTestWith, run)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
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
  test"quickcheck" do
    quickCheck theCommutativeProperty
  suite "a test suite" do
    test "a test in a test suite" do
      pure unit
  test "tests run only once: part 1" do
    liftEff $ Ref.modifyRef ref (_ + 1)
  test "tests run only once: part deux" do
    Assert.equal 1 =<< liftEff (Ref.readRef ref)
  suite "another suite" do
    test "this should not run" do
      pure unit
    test "a test in another test suite" do
      pure unit

main :: forall e. Eff (avar :: AVAR, console :: CONSOLE, random :: RANDOM, ref :: Ref.REF, testOutput :: TESTOUTPUT | e) Unit
main = run do
  ref <- liftEff $ Ref.newRef 0
  runTestWith Fancy.runTest $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith Simple.runTest $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith TAP.runTest $ tests ref

  var1 <- liftEff $ Ref.newRef 0
  runTestWith Fancy.runTest do
    testSkip "skip 1" do
      failure "skippedTestError"
    suiteSkip "skipped suite" do
      test "skip 2" do
        failure "skippedTestError"
      suite "also skipped" do
        test "skip 3" do
          failure "skippedTestError"
    suite "not skipped suite" do
      test "not skipped test" do
        liftEff $ Ref.modifyRef var1 (add 1)
        pure unit
      suiteSkip "skipped subsuite" do
        test "skip 3" do
          failure "skippedTestError"
      suite "not skipped inner suite" do
        test "also not skipped" do
          liftEff $ Ref.modifyRef var1 (add 1)
          pure unit

  runTestWith Fancy.runTest do
    test "all not skipped tests were executed" do
      val <- liftEff $ Ref.readRef var1
      Assert.equal 2 val

  var2 <- liftEff $ Ref.newRef 0
  runTestWith Fancy.runTest do
    testOnly "only 1" do
      liftEff $ Ref.modifyRef var2 (add 1)
      pure unit
    test "skipped 1" do
      failure "onlyTestError"
    suite "suite" do
      test "skipped 2" do
        failure "onlyTestError"
      testOnly "only 2" do
        liftEff $ Ref.modifyRef var2 (add 1)
        pure unit
    suite "empty suite" do
      test "skipped 3" do
        failure "onlyTestError"
      test "skipped 4" do
        failure "onlyTestError"

  runTestWith Fancy.runTest do
    test "all `testOnly` tests where executed" do
      val <- liftEff $ Ref.readRef var2
      Assert.equal 2 val

  var3 <- liftEff $ Ref.newRef false
  runTestWith Fancy.runTest do
    suiteOnly "only suite" do
      test "test 1" do
        pure unit
    suite "skipped suite" do
      test "skipped test" do
        failure "onlyTestError"
      suiteOnly "only suite 2" do
        test "this test will run" do
          liftEff $ Ref.writeRef var3 true
          pure unit

  runTestWith Fancy.runTest do
    test "inner suite run" do
      val <- liftEff $ Ref.readRef var3
      Assert.assert "did run" val
