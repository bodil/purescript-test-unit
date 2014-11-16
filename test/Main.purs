module Test.Main where

import Control.Monad.Cont.Trans
import Test.Unit

main = runTest do
  test "basic asserts" do
    assert "wasn't true" true
    assertFalse "wasn't false" false
  test "async asserts" do
    testC $ ContT \done -> done success
    testFn \done -> done success
    assertC "ContT didn't yield true" $ ContT \done -> done true
    assertFn "callback didn't receive true" \done -> done true
