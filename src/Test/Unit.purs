module Test.Unit
  ( TestUnit(..)
  , Test(..)
  , Assertion(..)
  , TestResult(..)
  , success
  , failure
  , assert
  , assertFalse
  , testC
  , testFn
  , assertC
  , assertFn
  , test
  , runTest
  ) where

import Control.Monad.Cont.Trans
import Control.Monad.Eff
import Control.Monad.Error.Trans
import Data.Either
import Test.Unit.Console

type TestResult = Either String Unit
type TestUnit e = ErrorT String (ContT Unit (Eff e)) Unit
type Assertion e = TestUnit e
type Test e = TestUnit e

success :: TestResult
success = Right unit

failure :: String -> TestResult
failure = Left

assert :: forall e. String -> Boolean -> Assertion e
assert _ true = ErrorT $ ContT \cb -> cb success
assert reason false = ErrorT $ ContT \cb -> cb $ failure reason

assertFalse :: forall e. String -> Boolean -> Assertion e
assertFalse _ false = ErrorT $ ContT \cb -> cb success
assertFalse reason true = ErrorT $ ContT \cb -> cb $ failure reason

testC :: forall e. (ContT Unit (Eff e) TestResult) -> Assertion e
testC c = ErrorT $ ContT \cb -> runContT c cb

testFn :: forall e. ((TestResult -> Eff e Unit) -> Eff e Unit) -> Assertion e
testFn f = testC $ ContT f

assertC :: forall e. String -> (ContT Unit (Eff e) Boolean) -> Assertion e
assertC reason c = ErrorT $ ContT \cb -> runContT c \res -> if res then cb (Right unit) else cb (Left reason)

assertFn :: forall e. String -> ((Boolean -> Eff e Unit) -> Eff e Unit) -> Assertion e
assertFn reason f = assertC reason $ ContT f

test :: forall e. String -> Assertion (testOutput :: TestOutput | e) -> Test (testOutput :: TestOutput | e)
test l t =
  ErrorT $ ContT run
  where run cb = do
          savePos
          print "→ Running: "
          printLabel l
          runContT (runErrorT t) handler
          where handler (Right _) = do
                  restorePos
                  eraseLine
                  printPass "✓ Passed: "
                  printLabel l
                  print "\n"
                  cb success
                handler error@(Left reason) = do
                  restorePos
                  eraseLine
                  printFail "☠ Failed: "
                  printLabel l
                  print " because "
                  printFail reason
                  print "\n"
                  cb error

foreign import exit """
  function exit(rv) {
    return function() {
      process.exit(rv);
    }
  }""" :: forall e. Number -> Eff (testOutput :: TestOutput | e) Unit

runTest :: forall e. Test (testOutput :: TestOutput | e) -> Eff (testOutput :: TestOutput | e) Unit
runTest t =
  runContT (runErrorT t) handler
  where handler (Left reason) = exit 1
        handler _ = return unit
