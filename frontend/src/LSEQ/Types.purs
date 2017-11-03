module LSEQ.Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe)

type Container a b = {
  id :: Maybe a
, payload :: Maybe b
, subtree :: CharTree a b
}

data AllocType = Plus | Minus

data Position = N Int | End

type TreeBody a b = {
  items :: Map Int (Container a b)
, allocType :: AllocType
}

data CharTree a b = Leaf | CharTree (TreeBody a b)

class CharTreeDisplay a where
  displayElement :: a -> String

instance functorCharTree :: Functor (CharTree a) where
  map f Leaf = Leaf
  map f (CharTree tree@{items}) = CharTree tree {
    items = map (\i -> i {payload = map f i.payload, subtree = map f i.subtree}) items
  }

-- TODO:
-- [ ] Add a foldable instance
-- [ ] Add a traversable instance
-- [ ] Move utility functions to these instances
