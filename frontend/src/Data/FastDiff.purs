module Data.FastDiff (
  diff
, OpType(..)
) where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Tuple (Tuple(..))

data OpType = Insert | Equal | Delete

foreign import rawDiff :: Fn2 String String (Array (Tuple OpType String))

diff :: String -> String -> Array (Tuple OpType String)
diff = runFn2 rawDiff
