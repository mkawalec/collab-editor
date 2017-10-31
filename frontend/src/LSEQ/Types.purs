module LSEQ.Types where

import Data.Map (Map)
import Data.List (List)

type Letter a b = {
  letter :: Char
, meta :: a
, id :: b
, subtree :: CharTree a b
}

data AllocType = Plus | Minus

type TreeBody a b = {
  chars :: Map Int (Letter a b)
, allocType :: AllocType
}

data CharTree a b = Leaf | CharTree (TreeBody a b)

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

capacity :: Int
capacity = 50
