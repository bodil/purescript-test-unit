module Test.Unit
  ( TestUnit(..)
  , Test(..)
  , Assertion(..)
  , TestResult(..)
  , success
  , failure
  , pickFirst
  , timeout
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
import Control.Monad.Eff.Ref
import Control.Monad.Error.Trans
import qualified Control.Reactive.Timer as Timer
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

pickFirst :: forall e. TestUnit (ref :: Ref | e) -> TestUnit (ref :: Ref | e) -> TestUnit (ref :: Ref | e)
pickFirst t1 t2 = ErrorT $ ContT $ \cb -> do
  yielded <- newRef false
  let yield t = runContT (runErrorT t) \res -> do
        hasYielded <- readRef yielded
        writeRef yielded true
        if hasYielded then return unit else cb res
  yield t1
  yield t2

timeout :: forall e. Number -> TestUnit (timer :: Timer.Timer, ref :: Ref | e) -> TestUnit (timer :: Timer.Timer, ref :: Ref | e)
timeout time test = pickFirst test $ ErrorT $ ContT \cb -> do
  Timer.timeout time $ do cb $ failure $ "test timed out after " ++ show time ++ "ms"
  return unit

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
        handler _ = exit 0
