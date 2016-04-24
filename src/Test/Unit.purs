module Test.Unit
  ( TestUnit(..)
  , Test(..)
  , Assertion(..)
  , TIMER(..)
  , success
  , failure
  , timeout
  , test
  , runTest
  ) where

import Prelude
import Control.Monad.Aff (Aff, runAff, attempt, forkAff, makeAff, cancelWith)
import Control.Monad.Aff.AVar (AVAR, killVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message)
import Data.Either (Either(..), either)
import Test.Unit.Console (hasColours, TESTOUTPUT, hasStderr, consoleError, consoleLog, printLabel, print, savePos, printFail, eraseLine, restorePos, printPass)

foreign import data TIMER :: !

type TestUnit e = Aff (timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT | e) Unit
-- type TestUnit e = ExceptT String (ContT Unit (Eff e)) Unit
type Assertion e = TestUnit e
type Test e = TestUnit e

-- | The basic value for a succeeding test.
success :: forall e. TestUnit e
success = makeAff \_ succeed -> succeed unit

-- | Make a failing test, given a reason for the failure.
failure :: forall e. String -> TestUnit e
failure reason = makeAff \fail _ -> fail $ error reason

foreign import setTimeout :: forall e a. Int -> Eff (timer :: TIMER | e) a -> Eff (timer :: TIMER | e) Unit

makeTimeout :: forall e. Int -> Aff (timer :: TIMER | e) Unit
makeTimeout time = makeAff \fail _ -> setTimeout time $ fail $ error $ "test timed out after " ++ show time ++ "ms"

pickFirst :: forall a e. Par a e -> Par a e -> Par a e
pickFirst (Par a1) (Par a2) =
  Par do
    va <- makeVar
    c1 <- forkAff $ attempt a1 >>= either (killVar va) (putVar va)
    c2 <- forkAff $ attempt a2 >>= either (killVar va) (putVar va)
    (takeVar va) `cancelWith` (c1 <> c2)

-- | Set a test to fail after a given number of milliseconds.
timeout :: forall e. Int -> TestUnit e -> TestUnit e
timeout time t = runPar $ Par t `pickFirst` Par (makeTimeout time)

runWithStderr :: forall e. String -> TestUnit e -> TestUnit e
runWithStderr label t = do
  liftEff $ do
    savePos
    print "\x2192 Running: "
    printLabel label
    restorePos
  attempt t >>= handler
  where handler (Right _) = liftEff $ do
          eraseLine
          printPass "\x2713 Passed: "
          printLabel label
          print "\n"
        handler (Left err) = do
          let reason = message err
          liftEff $ do
            eraseLine
            printFail "\x2620 Failed: "
            printLabel label
            print " because "
            printFail reason
            print "\n"
          failure reason

runWithConsole :: forall e. String -> TestUnit e -> TestUnit e
runWithConsole l t =
  attempt t >>= handler
  where handler (Right _) = do
          liftEff $ consoleLog $ "\x2713 Passed: " ++ l
        handler (Left err) = do
          let reason = message err
          liftEff $ consoleError $ "\x2620 Failed: " ++ l ++ " because " ++ reason

-- | Make a named test.
test :: forall e. String -> Assertion e -> Test e
test l t =
  if hasStderr && hasColours then runWithStderr l t else runWithConsole l t

foreign import exit :: forall e. Int -> Eff (testOutput :: TESTOUTPUT | e) Unit

-- | Run a test suite, and exit the process when it's done, setting the
-- | appropriate return code.
runTest :: forall e. Test e -> Eff (timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT | e) Unit
runTest =
  runAff errorHandler successHandler
  where errorHandler _ = exit 1
        successHandler _ = exit 0
