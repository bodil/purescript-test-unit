module Test.Main where

import Prelude
import Effect.Aff (never)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, failure, suite, suiteOnly, suiteSkip, test, testOnly, testSkip, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith, run)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Test.Unit.QuickCheck (quickCheck)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

modify' :: forall a. (a -> a) -> Ref.Ref a -> Effect Unit
modify' m s = void $ Ref.modify m s

tests :: Ref.Ref Int -> TestSuite
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
    liftEffect $ modify' (_ + 1) ref
  test "tests run only once: part deux" do
    Assert.equal 1 =<< liftEffect (Ref.read ref)
  suite "another suite" do
    test "this should not run" do
      pure unit
    test "a test in another test suite" do
      pure unit

main :: Effect Unit
main = run do
  ref <- liftEffect $ Ref.new 0
  runTestWith Fancy.runTest $ tests ref
  liftEffect $ Ref.write 0 ref
  runTestWith Simple.runTest $ tests ref
  liftEffect $ Ref.write 0 ref
  runTestWith TAP.runTest $ tests ref

  var1 <- liftEffect $ Ref.new 0
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
        liftEffect $ modify' (add 1) var1
        pure unit
      suiteSkip "skipped subsuite" do
        test "skip 3" do
          failure "skippedTestError"
      suite "not skipped inner suite" do
        test "also not skipped" do
          liftEffect $ modify' (add 1) var1
          pure unit

  runTestWith Fancy.runTest do
    test "all not skipped tests were executed" do
      val <- liftEffect $ Ref.read var1
      Assert.equal 2 val

  var2 <- liftEffect $ Ref.new 0
  runTestWith Fancy.runTest do
    testOnly "only 1" do
      liftEffect $ modify' (add 1) var2
      pure unit
    test "skipped 1" do
      failure "onlyTestError"
    suite "suite" do
      test "skipped 2" do
        failure "onlyTestError"
      testOnly "only 2" do
        liftEffect $ modify' (add 1) var2
        pure unit
    suite "empty suite" do
      test "skipped 3" do
        failure "onlyTestError"
      test "skipped 4" do
        failure "onlyTestError"

  runTestWith Fancy.runTest do
    test "all `testOnly` tests where executed" do
      val <- liftEffect $ Ref.read var2
      Assert.equal 2 val

  var3 <- liftEffect $ Ref.new false
  runTestWith Fancy.runTest do
    suiteOnly "only suite" do
      test "test 1" do
        pure unit
    suite "skipped suite" do
      test "skipped test" do
        failure "onlyTestError"
      suiteOnly "only suite 2" do
        test "this test will run" do
          liftEffect $ Ref.write true var3
          pure unit

  runTestWith Fancy.runTest do
    test "inner suite run" do
      val <- liftEffect $ Ref.read var3
      Assert.assert "did run" val
