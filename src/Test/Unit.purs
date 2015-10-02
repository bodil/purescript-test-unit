module Test.Unit
  ( TestUnit(..)
  , Test(..)
  , Assertion(..)
  , TestResult(..)
  , Timer(..)
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
  , setTimeout
  ) where

import Prelude
import Control.Monad.Cont.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Except.Trans
import Data.Either
import Test.Unit.Console

type TestResult = Either String Unit
type TestUnit e = ExceptT String (ContT Unit (Eff e)) Unit
type Assertion e = TestUnit e
type Test e = TestUnit e

success :: TestResult
success = Right unit

failure :: String -> TestResult
failure = Left

pickFirst :: forall e. TestUnit (ref :: REF | e) -> TestUnit (ref :: REF | e) -> TestUnit (ref :: REF | e)
pickFirst t1 t2 = ExceptT $ ContT $ \cb -> do
  yielded <- newRef false
  let yield t = runContT (runExceptT t) \res -> do
        hasYielded <- readRef yielded
        writeRef yielded true
        if hasYielded then return unit else cb res
  yield t1
  yield t2


foreign import data Timer :: !

foreign import setTimeout :: forall e a. Int -> Eff (timer :: Timer | e) a -> Eff (timer :: Timer | e) Unit

timeout :: forall e. Int -> TestUnit (timer :: Timer, ref :: REF | e) -> TestUnit (timer :: Timer, ref :: REF | e)
timeout time t = pickFirst t $
  ExceptT $ ContT \cb -> do
  setTimeout time $ do cb $ failure $ "test timed out after " ++ show time ++ "ms"
  return unit

assert :: forall e. String -> Boolean -> Assertion e
assert _ true = ExceptT $ ContT \cb -> cb success
assert reason false = ExceptT $ ContT \cb -> cb $ failure reason

assertFalse :: forall e. String -> Boolean -> Assertion e
assertFalse _ false = ExceptT $ ContT \cb -> cb success
assertFalse reason true = ExceptT $ ContT \cb -> cb $ failure reason

testC :: forall e. (ContT Unit (Eff e) TestResult) -> Assertion e
testC c = ExceptT $ ContT \cb -> runContT c cb

testFn :: forall e. ((TestResult -> Eff e Unit) -> Eff e Unit) -> Assertion e
testFn f = testC $ ContT f

assertC :: forall e. String -> (ContT Unit (Eff e) Boolean) -> Assertion e
assertC reason c = ExceptT $ ContT \cb -> runContT c \res -> if res then cb (Right unit) else cb (Left reason)

assertFn :: forall e. String -> ((Boolean -> Eff e Unit) -> Eff e Unit) -> Assertion e
assertFn reason f = assertC reason $ ContT f

runWithStderr l t cb = do
  savePos
  print "\x2192 Running: "
  printLabel l
  runContT (runExceptT t) handler
  where handler (Right _) = do
          restorePos
          eraseLine
          printPass "\x2713 Passed: "
          printLabel l
          print "\n"
          cb success
        handler error@(Left reason) = do
          restorePos
          eraseLine
          printFail "\x2620 Failed: "
          printLabel l
          print " because "
          printFail reason
          print "\n"
          cb error

runWithConsole l t cb =
  runContT (runExceptT t) handler
  where handler (Right _) = do
          consoleLog $ "\x2713 Passed: " ++ l
          cb success
        handler error@(Left reason) = do
          consoleError $ "\x2620 Failed: " ++ l ++ " because " ++ reason
          cb error

test :: forall e. String -> Assertion (testOutput :: TestOutput | e) -> Test (testOutput :: TestOutput | e)
test l t =
  ExceptT $ ContT if hasStderr then runWithStderr l t else runWithConsole l t

foreign import exit :: forall e. Int -> Eff (testOutput :: TestOutput | e) Unit

runTest :: forall e. Test (testOutput :: TestOutput | e) -> Eff (testOutput :: TestOutput | e) Unit
runTest t = do
  runContT (runExceptT t) handler
  where handler (Left reason) = exit 1
        handler _ = exit 0
