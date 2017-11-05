module Data.FastDiff (
  diff
, OpType(..)
) where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)

data OpType = Insert | Equal | Delete

foreign import rawDiff :: forall a b.
  Fn3 (a -> b -> Tuple a b) String String (Array (Tuple Int String))

diff :: String -> String -> Array (Tuple OpType String)
diff a b = map withTypedOp result
    where result = runFn3 rawDiff Tuple a b
          opToType = case _ of
            1 -> Insert
            0 -> Equal
            -1 -> Delete
            _ -> Delete
          withTypedOp = bimap opToType id
