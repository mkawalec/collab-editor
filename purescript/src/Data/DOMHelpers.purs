module Data.DOMHelpers (
  value
, setValue
) where

import Prelude
import DOM (DOM)
import DOM.Node.Types (Node)
import Control.Monad.Eff (Eff)

foreign import value :: forall e. Node -> Eff (dom :: DOM | e) String

foreign import setValue :: forall e. String -> Node -> Eff (dom :: DOM | e) Unit
