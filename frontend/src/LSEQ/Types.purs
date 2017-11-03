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
derive instance eqAllocType :: Eq AllocType

data Position = N Int | End
derive instance eqPosition :: Eq Position

type TreeBody a b = {
  items :: Map Int (Container a b)
, allocType :: AllocType
}

data CharTree a b = Leaf | CharTree (TreeBody a b)

class CharTreeDisplay a where
  displayElement :: a -> String

derive instance functorCharTree :: Functor (CharTree a)

-- TODO:
-- [ ] Add a foldable instance
-- [ ] Add a traversable instance
-- [ ] Move utility functions to these instances
