module Test.Unit.Console where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC

log :: forall a. MonadEffect a => String -> a Unit
log = liftEffect <<< EC.log

foreign import hasStderr :: Boolean

foreign import hasColours :: Boolean

foreign import consoleLog :: String -> Effect Unit

foreign import consoleError :: String -> Effect Unit

foreign import savePos :: Effect Unit

foreign import restorePos :: Effect Unit

foreign import eraseLine :: Effect Unit

foreign import print :: String -> Effect Unit

foreign import printLabel:: String -> Effect Unit

foreign import printFail :: String -> Effect Unit

foreign import printPass :: String -> Effect Unit
