module Data.LSEQ.Types where

import Data.Map (Map)

import GHC.Generics

data CharTree a b = Leaf | CharTree (TreeBody a b)
  deriving (Show, Eq, Ord, Generic)

-- | Tree is an LSEQ tree inner container. It holds
--   an arbitrary amount of children mapped by child ids
--   If AllocAffinity is Left, children will be more likely
--   to appear on the left of the available range
data TreeBody a b = TreeBody {
  items :: Map Int (Container a b)
, allocAffinity :: AllocAffinity
} deriving (Show, Eq, Ord, Generic)

data Container a b = Container {
  containerId :: Maybe a
, payload :: Maybe b
, subtree :: CharTree a b
} deriving (Show, Eq, Ord, Generic)

data AllocAffinity = Left | Right deriving (Show, Eq, Ord, Generic)

startingCapacity :: Int
startingCapacity = 20
