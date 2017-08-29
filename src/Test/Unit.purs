module Test.Unit
  ( Test(..)
  , TestEffects(..)
  , TestF(..)
  , Group(..)
  , TestSuite
  , TestList
  , Skip(..)
  , Only(..)
  , success
  , failure
  , timeout
  , test
  , testOnly
  , testSkip
  , suite
  , suiteOnly
  , suiteSkip
  , walkSuite
  , filterTests
  , collectTests
  , collectResults
  , countSkippedTests
  , keepErrors
  , describe
  , it
  ) where

import Prelude

import Control.Monad.Aff (Aff, attempt, makeAff, forkAff, cancelWith)
import Control.Monad.Aff.AVar (modifyVar, makeVar', makeVar, killVar, putVar, takeVar, AVAR)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Free (Free, foldFree, liftF, runFreeM, substFree)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Data.Either (Either(Left), either)
import Data.Foldable (foldl)
import Data.HeytingAlgebra (ff, implies)
import Data.List (snoc, List(Cons, Nil))
import Data.Newtype (class Newtype, over, over2, un)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Test.Unit.Console (TESTOUTPUT)

foreign import memoise :: forall a e. Aff e a -> Aff e a

type Test e = Aff e Unit
type TestEffects e = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e)

-- | The basic value for a succeeding test.
success :: forall e. Test e
success = makeAff \_ succeed -> succeed unit

-- | Make a failing test, given a reason for the failure.
failure :: forall e. String -> Test e
failure reason = makeAff \fail _ -> fail $ error reason

makeTimeout :: forall e. Int -> Aff (timer :: TIMER | e) Unit
makeTimeout time = makeAff \fail _ -> void $ setTimeout time $ fail $ error $ "test timed out after " <> show time <> "ms"

pickFirst :: forall e. Test (avar :: AVAR | e) -> Test (avar :: AVAR | e) -> Test (avar :: AVAR | e)
pickFirst t1 t2 = do
  va <- makeVar
  c1 <- forkAff $ attempt t1 >>= either (killVar va) (putVar va)
  c2 <- forkAff $ attempt t2 >>= either (killVar va) (putVar va)
  (takeVar va) `cancelWith` (c1 <> c2)

-- | Set a test to fail after a given number of milliseconds.
timeout :: forall e. Int -> Test (timer :: TIMER, avar :: AVAR | e) -> Test (timer :: TIMER, avar :: AVAR | e)
timeout time t = t `pickFirst` (makeTimeout time)

newtype Skip = Skip Boolean

derive instance newtypeSkip :: Newtype Skip _

newtype Only = Only Boolean

derive instance newtypeOnly :: Newtype Only _

instance showOnly :: Show Only where
  show (Only b) = show b

instance haytingAlgebraOnly :: HeytingAlgebra Only where
  ff = Only false
  tt = Only true
  implies = over2 Only implies
  conj = over2 Only conj
  disj = over2 Only disj
  not = over Only not

data Group e = Group String (TestSuite e)

data TestF e a = TestGroup (Group e) Skip Only a
               | TestUnit String Skip Only (Test e) a
               | SkipUnit (TestF e a) a

type TestSuite e = Free (TestF e) Unit

skipUnit :: forall a e. (TestF e a) -> a -> Free (TestF e) a
skipUnit t a = liftF <<< SkipUnit t $ a

instance functorTestF :: Functor (TestF e) where
  map f (TestGroup g s o a) = TestGroup g s o (f a)
  map f (TestUnit l s o t a) = TestUnit l s o t (f a)
  map f (SkipUnit t a) = SkipUnit (f <$> t) (f a)

-- | Define a test suite, which can contain a number of nested suites
-- | as well as tests.
suite :: forall e. String -> TestSuite e -> TestSuite e
suite label tests = liftF $ TestGroup (Group label tests) (Skip false) (Only false) unit

-- | Run only this suite.
suiteOnly :: forall e. String -> TestSuite e -> TestSuite e
suiteOnly label tests = liftF $ TestGroup (Group label tests) (Skip false) (Only true) unit

-- | Skip this suite.
suiteSkip :: forall e. String -> TestSuite e -> TestSuite e
suiteSkip label tests = liftF $ TestGroup (Group label tests) (Skip true) (Only false) unit

-- | Define a labelled test.
test :: forall e. String -> Test e -> TestSuite e
test l t = liftF $ TestUnit l (Skip false) (Only false) (memoise t) unit

-- | Run only this test.
testOnly :: forall e. String -> Test e -> TestSuite e
testOnly l t = liftF $ TestUnit l (Skip false) (Only true) (memoise t) unit

-- | Skip a test.
testSkip :: forall e. String -> Test e -> TestSuite e
testSkip l t = liftF $ TestUnit l (Skip true) (Only false) (memoise t) unit

-- | Find if there are suites and tests with `Only true` flag.
-- | Returns a tuple where the first factor is `Only true` iff there is a suite
-- | with only flag set to true, and the second factor is `Only true` iff there
-- | is a test with only flag set to true.
hasOnly :: forall a e. Free (TestF e) a -> Tuple Only Only
hasOnly t = execState (foldFree go t) ff
  where
    go :: TestF e ~> State (Tuple Only Only)
    go (TestGroup (Group _ t') _ only r) = modify (disj (hasOnly t') <<< disj (Tuple only ff)) $> r
    go (TestUnit _ _ only _ r) = modify (disj (Tuple ff only)) $> r
    go (SkipUnit _ r) = pure r

filterEmptyNodes :: forall e. Free (TestF e) ~> Free (TestF e)
filterEmptyNodes = substFree go
  where

    go :: TestF e ~> Free (TestF e)
    go tg@(TestGroup (Group _ t) _ _ r) | isEmpty t = skipUnit tg r
                                        | otherwise = liftF tg
    go t = liftF t

    isEmpty :: forall a. Free (TestF e) a -> Boolean
    isEmpty t = execState (foldFree empty t) true

    empty :: TestF e ~> State Boolean 
    empty tg@(TestGroup (Group _ t) _ _ r) = modify (conj $ isEmpty t) $> r
    empty (TestUnit _ _ _ _ r) = modify (conj false) $> r
    empty (SkipUnit _ r) = pure r

countTests :: forall a e. Free (TestF e) a -> Int
countTests ts = execState (foldFree go ts) 0
  where
    go :: TestF e ~> State Int
    go (SkipUnit _ a) = pure a
    go (TestUnit _ _ _ _ a) = modify (add 1) $> a
    go (TestGroup (Group _ t) _ _ a) = modify (add (countTests t)) $> a

countSkippedTests :: forall a e. Free (TestF e) a -> Int
countSkippedTests ts = execState (foldFree go ts) 0
  where
    go :: TestF e ~> State Int
    go (SkipUnit (TestUnit _ _ _ _ _) a) = modify (add 1) $> a
    go (SkipUnit (TestGroup (Group _ t) _ _ _) a) = modify (add (countTests t)) $> a
    go (SkipUnit (SkipUnit _ _) a) = pure a
    go (TestUnit _ _ _ _ a) = pure a
    go (TestGroup (Group _ t) _ _ a) = modify (add (countSkippedTests t)) $> a

-- | Filter suites and tests with `Only` and `Skip` flags and removes suites
-- | that do not contain any tests.
filterTests :: forall e. Free (TestF e) ~> Free (TestF e)
filterTests t =
  let Tuple os ot = hasOnly t

      go :: Only -> TestF e ~> Free (TestF e)
      go _ tg@(TestGroup (Group n t') s o a)
        = if un Skip s
            then skipUnit tg a
            else liftF $ TestGroup (Group n (substFree (go o) t')) s o a
      go inOnly tu@(TestUnit n s o t' a)
        = case un Only (os `implies` inOnly && ot `implies` o) && not (un Skip s) of
            true  -> liftF tu
            false -> skipUnit tu a
      go _ su@(SkipUnit _ _)  = liftF su

  in filterEmptyNodes $ substFree (go (Only false)) t

-- | A list of collected tests, represented as a tuple of each test's path
-- | and the `Test` itself. The path, in this case, means the name of the
-- | test preceded by the name of each parent test suite, in top down order.
type TestList e = List (Tuple (List String) (Test e))

-- | Walk through a test suite, calling the provided function for each item,
-- | and returning a `TestList` of all tests walked. The tests won't actually
-- | run unless you run them explicitly from your walker function.
walkSuite :: forall e. (List String -> TestF (avar :: AVAR | e) (TestSuite (avar :: AVAR | e)) -> Aff (avar :: AVAR | e) Unit) -> TestSuite (avar :: AVAR | e) -> Aff (avar :: AVAR | e) (TestList (avar :: AVAR | e))
walkSuite runItem tests = do
  coll <- makeVar' Nil
  let walkItem path group@(TestGroup (Group label content) skip only rest) = do
        runItem path group
        runFreeM (walkItem (snoc path label)) content
        pure rest
      walkItem path t@(TestUnit label skip only aff rest) = do
        modifyVar (Cons (Tuple (snoc path label) aff)) coll
        runItem path t
        pure rest
      walkItem path (SkipUnit _ rest) = do
        pure rest
  runFreeM (walkItem Nil) tests
  res <- takeVar coll
  pure res

-- | Walk through a test suite, returning a `TestList` of all tests walked.
-- | This operation will not actually run the tests.
collectTests :: forall e. TestSuite (avar :: AVAR | e) -> Aff (avar :: AVAR | e) (TestList (avar :: AVAR | e))
collectTests t = walkSuite (\_ _ -> pure unit) t

-- | Run a list of tests and collect each test result.
collectResults :: forall e. TestList e -> Aff e (List (Tuple (List String) (Either Error Unit)))
collectResults tests = for tests run
  where run (Tuple label t) = Tuple label <$> attempt t

-- | Filter successes out of a list of test results.
keepErrors :: List (Tuple (List String) (Either Error Unit)) -> List (Tuple (List String) Error)
keepErrors = foldl run Nil
  where run s (Tuple label (Left err)) = snoc s $ Tuple label err
        run s _ = s


-- Some aliases to keep Dan North happy.

-- | `describe` is an alias for `suite` for BDD enthusiasts.
describe :: forall e. String -> TestSuite e -> TestSuite e
describe = suite

-- | `fdescribe` is an alias for `suiteOnly`.
fdescribe :: forall e. String -> TestSuite e -> TestSuite e
fdescribe = suiteOnly

-- | `xdescribe` is an alias for `suiteSkip`.
xdescribe :: forall e. String -> TestSuite e -> TestSuite e
xdescribe = suiteSkip

-- | `it` is an alias for `test` for BDD enthusiasts.
it :: forall e. String -> Test e -> TestSuite e
it = test

-- | `fit` is an alias for `testOnly`.
fit :: forall e. String -> Test e -> TestSuite e
fit = testOnly

-- | `xit` is an alias for `testSkip`.
xit :: forall e. String -> Test e -> TestSuite e
xit = testSkip
