module LSEQ.Types where

import Prelude
import Data.List (List)
import Data.Map (Map)

type Container a b = {
  id :: a
, payload :: b
, subtree :: CharTree a b
}

data AllocType = Plus | Minus

type TreeBody a b = {
  items :: Map Int (Container a b)
, allocType :: AllocType
}

data CharTree a b = Leaf | CharTree (TreeBody a b)

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

capacity :: Int
capacity = 50

instance functorCharTree :: Functor (CharTree a) where
  map f Leaf = Leaf
  map f (CharTree tree@{items}) = CharTree tree {
    items = map (\i -> i {payload = f i.payload, subtree = map f i.subtree}) items
  }
