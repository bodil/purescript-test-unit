module Test.Unit.Console where

import Prelude
import Control.Monad.Eff

foreign import data TestOutput :: !

foreign import hasStderr :: Boolean

foreign import consoleLog :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import consoleError :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import savePos :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import restorePos :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import eraseLine :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import print :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printLabel:: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printFail :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printPass :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit
