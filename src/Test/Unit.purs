module Test.Unit
  ( Test(..)
  , TestF(..)
  , Group(..)
  , TestSuite
  , TestList
  , success
  , failure
  , timeout
  , test
  , suite
  , walkSuite
  , collectTests
  , collectResults
  , keepErrors
  , describe
  , it
  ) where

import Prelude
import Control.Monad.Aff (Aff, attempt, makeAff, forkAff, cancelWith)
import Control.Monad.Aff.AVar (modifyVar, makeVar', makeVar, killVar, putVar, takeVar, AVAR)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Free (runFreeM, Free, liftF)
import Data.Either (Either(Left), either)
import Data.Foldable (foldl)
import Data.List (snoc, List(Cons, Nil))
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))

foreign import memoise :: forall a e. Aff e a -> Aff e a

type Test e = Aff e Unit

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



data Group e = Group String (TestSuite e)

data TestF e a = TestGroup (Group e) a
               | TestUnit String (Test e) a

type TestSuite e = Free (TestF e) Unit

instance functorTestF :: Functor (TestF e) where
  map f (TestGroup g a) = TestGroup g (f a)
  map f (TestUnit l t a) = TestUnit l t (f a)

-- | Define a test suite, which can contain a number of nested suites
-- | as well as tests.
suite :: forall e. String -> TestSuite e -> TestSuite e
suite label tests = liftF $ TestGroup (Group label tests) unit

-- | Define a labelled test.
test :: forall e. String -> Test e -> TestSuite e
test l t = liftF $ TestUnit l (memoise t) unit



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
  let walkItem path group@(TestGroup (Group label content) rest) = do
        runItem path group
        runFreeM (walkItem (snoc path label)) content
        pure rest
      walkItem path t@(TestUnit label aff rest) = do
        modifyVar (Cons (Tuple (snoc path label) aff)) coll
        runItem path t
        pure rest
  runFreeM (walkItem Nil) tests
  res <- takeVar coll
  pure res

-- | Walk through a test suite, returning a `TestList` of all tests walked.
-- | This operation will not actually run the tests.
collectTests :: forall e. TestSuite (avar :: AVAR | e) -> Aff (avar :: AVAR | e) (TestList (avar :: AVAR | e))
collectTests = walkSuite (\_ _ -> pure unit)

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

-- | `it` is an alias for `test` for BDD enthusiasts.
it :: forall e. String -> Test e -> TestSuite e
it = test
