module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, makeVar', modifyVar, putVar, tryTakeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Data.Maybe (Maybe(Just))
import Test.QuickCheck (Result, (===))
import Test.Unit (Test, TestSuite, failure, suite, suiteOnly, suiteSkip, test, testOnly, testSkip, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTestWith, run)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
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
  test"quickcheck" do
    quickCheck theCommutativeProperty
  suite "a test suite" do
    test "a test in a test suite" do
      pure unit
  suite "another suite" do
    test "this should not run" do
      pure unit
    test "a test in another test suite" do
      pure unit
  test "tests run only once" do
    ref <- incRef
    Assert.equal 1 ref

main :: forall e. Eff (timer :: TIMER, avar :: AVAR, console :: CONSOLE, random :: RANDOM, testOutput :: TESTOUTPUT | e) Unit
main = run do
  _ <- resetRef
  runTestWith Fancy.runTest tests
  _ <- resetRef
  runTestWith Simple.runTest tests
  _ <- resetRef
  runTestWith TAP.runTest tests
  _ <- resetRef

  var1 <- makeVar' 0
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
        modifyVar (add 1) var1
        pure unit
      suiteSkip "skipped subsuite" do
        test "skip 3" do
          failure "skippedTestError"
      suite "not skipped inner suite" do
        test "also not skipped" do
          modifyVar (add 1) var1
          pure unit

  runTestWith Fancy.runTest do
    test "all not skipped tests were executed" do
      val <- tryTakeVar var1
      Assert.equal (Just 2) val

  var2 <- makeVar' 0
  runTestWith Fancy.runTest do
    testOnly "only 1" do
      modifyVar (add 1) var2
      pure unit
    test "skipped 1" do
      failure "onlyTestError"
    suite "suite" do
      test "skipped 2" do
        failure "onlyTestError"
      testOnly "only 2" do
        modifyVar (add 1) var2
        pure unit
    suite "empty suite" do
      test "skipped 3" do
        failure "onlyTestError"
      test "skipped 4" do
        failure "onlyTestError"

  runTestWith Fancy.runTest do
    test "all `testOnly` tests where executed" do
      val <- tryTakeVar (var2 :: AVar Int)
      Assert.equal (Just 2) val

  var3 <- makeVar
  runTestWith Fancy.runTest do
    suiteOnly "only suite" do
      test "test 1" do
        pure unit
    suite "skipped suite" do
      test "skipped test" do
        failure "onlyTestError"
      suiteOnly "only suite 2" do
        test "this test will run" do
          putVar var3 true
          pure unit

  runTestWith Fancy.runTest do
    test "inner suite run" do
      val <- tryTakeVar (var3 :: AVar Boolean)
      Assert.equal (Just true) val
