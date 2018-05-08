module Test.Unit
  ( Test(..)
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
import Control.Alt ((<|>))
import Effect.Aff (Aff, attempt, throwError, delay, suspendAff, joinFiber)
import Effect.Aff.AVar (new, put, take)
import Effect.Exception (error, Error)
import Control.Monad.Free (Free, foldFree, liftF, runFreeM, substFree)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.HeytingAlgebra (ff, implies)
import Data.Int (toNumber)
import Data.List (snoc, List(Cons, Nil), reverse)
import Data.Newtype (class Newtype, over, over2, un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))

type Test = Aff Unit

-- | The basic value for a succeeding test.
success :: Test
success = pure unit

-- | Make a failing test, given a reason for the failure.
failure :: String -> Test
failure = throwError <<< error

makeTimeout :: forall a. Int -> Aff a
makeTimeout time = do
  delay $ Milliseconds $ toNumber time
  throwError $ error $ "test timed out after " <> show time <> "ms"

-- | Set a test to fail after a given number of milliseconds.
timeout :: Int -> Test -> Test
timeout time t = do
  r <- sequential $ parallel (attempt $ makeTimeout time) <|> parallel (attempt t)
  either throwError (const success) r

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

data Group = Group String TestSuite

data TestF a = TestGroup Group Skip Only a
               | TestUnit String Skip Only (Test) a
               | SkipUnit (TestF a) a

type TestSuite = Free TestF Unit

skipUnit :: forall a. (TestF a) -> a -> Free TestF a
skipUnit t a = liftF <<< SkipUnit t $ a

instance functorTestF :: Functor TestF where
  map f (TestGroup g s o a) = TestGroup g s o (f a)
  map f (TestUnit l s o t a) = TestUnit l s o t (f a)
  map f (SkipUnit t a) = SkipUnit (f <$> t) (f a)

-- | Define a test suite, which can contain a number of nested suites
-- | as well as tests.
suite :: String -> TestSuite -> TestSuite
suite label tests = liftF $ TestGroup (Group label tests) (Skip false) (Only false) unit

-- | Run only this suite.
suiteOnly :: String -> TestSuite -> TestSuite
suiteOnly label tests = liftF $ TestGroup (Group label tests) (Skip false) (Only true) unit

-- | Skip this suite.
suiteSkip :: String -> TestSuite -> TestSuite
suiteSkip label tests = liftF $ TestGroup (Group label tests) (Skip true) (Only false) unit

-- | Define a labelled test.
test :: String -> Test -> TestSuite
test l t = liftF $ TestUnit l (Skip false) (Only false) t unit

-- | Run only this test.
testOnly :: String -> Test -> TestSuite
testOnly l t = liftF $ TestUnit l (Skip false) (Only true) t unit

-- | Skip a test.
testSkip :: String -> Test -> TestSuite
testSkip l t = liftF $ TestUnit l (Skip true) (Only false) t unit

-- | Find if there are suites and tests with `Only true` flag.
-- | Returns a tuple where the first factor is `Only true` iff there is a suite
-- | with only flag set to true, and the second factor is `Only true` iff there
-- | is a test with only flag set to true.
hasOnly :: forall a. Free TestF a -> Tuple Only Only
hasOnly t = execState (foldFree go t) ff
  where
    go :: TestF ~> State (Tuple Only Only)
    go (TestGroup (Group _ t') _ only r) = modify (disj (hasOnly t') <<< disj (Tuple only ff)) $> r
    go (TestUnit _ _ only _ r) = modify (disj (Tuple ff only)) $> r
    go (SkipUnit _ r) = pure r

filterEmptyNodes :: Free TestF ~> Free TestF
filterEmptyNodes = substFree go
  where

    go :: TestF ~> Free TestF
    go tg@(TestGroup (Group _ t) _ _ r) | isEmpty t = skipUnit tg r
                                        | otherwise = liftF tg
    go t = liftF t

    isEmpty :: forall a. Free TestF a -> Boolean
    isEmpty t = execState (foldFree empty t) true

    empty :: TestF ~> State Boolean
    empty tg@(TestGroup (Group _ t) _ _ r) = modify (conj $ isEmpty t) $> r
    empty (TestUnit _ _ _ _ r) = modify (conj false) $> r
    empty (SkipUnit _ r) = pure r

countTests :: forall a. Free TestF a -> Int
countTests ts = execState (foldFree go ts) 0
  where
    go :: TestF ~> State Int
    go (SkipUnit _ a) = pure a
    go (TestUnit _ _ _ _ a) = modify (add 1) $> a
    go (TestGroup (Group _ t) _ _ a) = modify (add (countTests t)) $> a

countSkippedTests :: forall a. Free TestF a -> Int
countSkippedTests ts = execState (foldFree go ts) 0
  where
    go :: TestF ~> State Int
    go (SkipUnit (TestUnit _ _ _ _ _) a) = modify (add 1) $> a
    go (SkipUnit (TestGroup (Group _ t) _ _ _) a) = modify (add (countTests t)) $> a
    go (SkipUnit (SkipUnit _ _) a) = pure a
    go (TestUnit _ _ _ _ a) = pure a
    go (TestGroup (Group _ t) _ _ a) = modify (add (countSkippedTests t)) $> a

-- | Filter suites and tests with `Only` and `Skip` flags and removes suites
-- | that do not contain any tests.
filterTests :: Free TestF ~> Free TestF
filterTests t =
  let Tuple os ot = hasOnly t

      go :: Only -> TestF ~> Free TestF
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
type TestList = List (Tuple (List String) (Test))

-- | Walk through a test suite, calling the provided function for each item,
-- | and returning a `TestList` of all tests walked. The tests won't actually
-- | run unless you run them explicitly from your walker function.
walkSuite :: (List String -> Either String (Tuple String Test) -> Aff Unit) -> TestSuite -> Aff TestList
walkSuite runItem tests = do
  coll <- new Nil
  let walkItem path group@(TestGroup (Group label content) skip only rest) = do
        runItem path $ Left label
        runFreeM (walkItem (snoc path label)) content
        pure rest
      walkItem path t@(TestUnit label skip only aff rest) = do
        fiber <- suspendAff aff
        cs <- take coll
        put (Cons (Tuple (snoc path label) $ joinFiber fiber) cs) coll
        runItem path $ Right $ Tuple label $ joinFiber fiber
        pure rest
      walkItem path (SkipUnit _ rest) = do
        pure rest
  runFreeM (walkItem Nil) tests
  res <- take coll
  pure res

-- | Walk through a test suite, returning a `TestList` of all tests walked.
-- | This operation will not actually run the tests.
collectTests :: TestSuite -> Aff TestList
collectTests = map reverse <<< walkSuite (\_ _ -> pure unit)

-- | Run a list of tests and collect each test result.
collectResults :: TestList -> Aff (List (Tuple (List String) (Either Error Unit)))
collectResults tests = for tests run
  where run (Tuple label t) = Tuple label <$> attempt t

-- | Filter successes out of a list of test results.
keepErrors :: List (Tuple (List String) (Either Error Unit)) -> List (Tuple (List String) Error)
keepErrors = foldl run Nil
  where run s (Tuple label (Left err)) = snoc s $ Tuple label err
        run s _ = s


-- Some aliases to keep Dan North happy.

-- | `describe` is an alias for `suite` for BDD enthusiasts.
describe :: String -> TestSuite -> TestSuite
describe = suite

-- | `fdescribe` is an alias for `suiteOnly`.
fdescribe :: String -> TestSuite -> TestSuite
fdescribe = suiteOnly

-- | `xdescribe` is an alias for `suiteSkip`.
xdescribe :: String -> TestSuite -> TestSuite
xdescribe = suiteSkip

-- | `it` is an alias for `test` for BDD enthusiasts.
it :: String -> Test -> TestSuite
it = test

-- | `fit` is an alias for `testOnly`.
fit :: String -> Test -> TestSuite
fit = testOnly

-- | `xit` is an alias for `testSkip`.
xit :: String -> Test -> TestSuite
xit = testSkip
