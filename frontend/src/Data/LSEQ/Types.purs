module Data.LSEQ.Types where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe(..))

import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)

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

-- can't define applicative/monad because of the stochastic positons

instance foldableCharTree :: Foldable (CharTree a) where
  foldMap f Leaf = mempty
  foldMap f (CharTree tree) = foldMap (\i -> case i.payload of
    Nothing -> foldMap f i.subtree
    Just p  -> f p <> foldMap f i.subtree) tree.items

  foldl f acc Leaf = acc
  foldl f acc (CharTree tree) = foldl (\acc i -> case i.payload of
    Nothing -> foldl f acc i.subtree
    Just p  -> foldl f (f acc p) i.subtree) acc tree.items

  foldr f acc Leaf = acc
  foldr f acc (CharTree tree) = foldr (\i acc -> case i.payload of
    Nothing -> foldr f acc i.subtree
    Just p  -> f p $ foldr f acc i.subtree) acc tree.items

-- TODO:
-- [x] Add a foldable instance
-- [ ] Add a traversable instance
-- [ ] Move utility functions to these instances
