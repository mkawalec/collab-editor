module Data.FastDiff (
  diff
, OpType(..)
) where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)
import Data.Array as A
import Data.List (List)

data OpType = Insert | Equal | Delete
instance showOpType :: Show OpType where
  show Insert = "Insert"
  show Equal = "Equal"
  show Delete = "Delete"

foreign import rawDiff :: forall a b.
  Fn3 (a -> b -> Tuple a b) String String (Array (Tuple Int String))

diff :: String -> String -> List (Tuple OpType String)
diff a b = A.toUnfoldable $ map withTypedOp result
    where result = runFn3 rawDiff Tuple a b
          opToType = case _ of
            1 -> Insert
            0 -> Equal
            -1 -> Delete
            _ -> Delete
          withTypedOp = bimap opToType id
