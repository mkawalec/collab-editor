module LSEQ (
  insert
, delete
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

import LSEQ.Types (CharTree(..), Container, TreeBody, capacity)
import LSEQ.Helpers (getOffset, inBounds, newCharTree)


-- TODO
--
-- [x] - move into separate modules
-- [ ] - move to foldable/traversable instances

walkTree :: forall a b e. Container a b -> Tuple Int Int -> TreeBody a b -> Int -> List Int ->
            Eff (random :: RANDOM | e) (CharTree a b)
walkTree item coords tree@{items, allocType} idx xs =
  case M.lookup idx items of
    Nothing   -> pure $ CharTree $ tree {items = M.insert idx item items}
    Just char -> do
      subtree <- insert' item xs coords char.subtree
      let char' = char {subtree = subtree}
      pure $ CharTree $ tree {items = M.insert idx char' items}

insert' :: forall a b e. Container a b -> List Int -> Tuple Int Int -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert' item _ _ Leaf = newCharTree >>= case _ of
  (CharTree tree@{items, allocType}) -> do
    idx <- getOffset allocType 0 capacity
    pure $ CharTree $ tree {items = M.insert idx item items}
  Leaf -> pure $ Leaf -- this should never happen lol

insert' item Nil coords@(Tuple p q) (CharTree tree@{items, allocType}) =
  getOffset allocType p q >>= \idx -> case idx == 0 && p == 0 && q == 0 of
    true -> case M.lookup idx items of
            Just char -> do
              -- left side tree rotation
              -- insert the item as a first element of the bottom subtree,
              -- with a leaf subtree
              let char'   = char {subtree = Leaf}
                  lowestIndex = case char.subtree of
                    Leaf             -> capacity
                    (CharTree tree') -> fromMaybe capacity $ _.key <$> M.findMin tree'.items
                  insertBound = Tuple 0 (lowestIndex - 1)

              updatedTree <- insert char' Nil insertBound char.subtree
              let item' = item {subtree = updatedTree}

              pure $ CharTree $ tree {items = M.insert idx item' items}
            Nothing -> walkTree item coords tree idx Nil
    otherwise -> walkTree item coords tree idx Nil

insert' item (Cons x xs) coords (CharTree tree) = walkTree item coords tree x xs

insert :: forall a b e. Container a b -> List Int -> Tuple Int Int -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert item path (Tuple p q) tree =
  insert' item path (Tuple (inBounds p) (inBounds q)) tree

delete :: forall a b. List Int -> Int -> CharTree a b -> CharTree a b
delete _ _ Leaf = Leaf
delete Nil p (CharTree tree@{items}) = CharTree $ tree {items = M.delete p items}
delete (Cons x xs) p (CharTree tree@{items}) = case item of
    Nothing -> CharTree tree
    Just l  -> let withNewTree = l {subtree = delete xs p l.subtree} in
                  CharTree $ tree {items = M.insert x withNewTree items}
  where item = M.lookup x items
