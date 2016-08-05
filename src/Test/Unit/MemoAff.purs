module Test.Unit.MemoAff (memoise) where

import Control.Monad.Aff (Aff)

foreign import memoise :: forall a e. Aff e a -> Aff e a
