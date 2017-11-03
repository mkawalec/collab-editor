module LSEQ (
  insert
, delete
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import LSEQ.Types (CharTree(..), Container, TreeBody, capacity)
import LSEQ.Helpers (getOffset, inBounds, newCharTree)


walkTree :: forall a b e. Container a b -> Tuple Int Int -> Int -> List Int -> TreeBody a b ->
            Eff (random :: RANDOM | e) (CharTree a b)
walkTree item coords idx xs tree@{items, allocType} =
  case M.lookup idx items of
    Nothing   -> do --pure $ CharTree $ tree {items = M.insert idx item items}
      subtree <- CharTree <$> newCharTree >>= insert' item xs coords
      let container = {id: Nothing, payload: Nothing, subtree: subtree}
      pure $ CharTree $ tree {items = M.insert idx container items}
    Just char -> do
      subtree <- insert' item xs coords char.subtree
      let char' = char {subtree = subtree}
      pure $ CharTree $ tree {items = M.insert idx char' items}

insertAtOffset :: forall a b e. Container a b -> TreeBody a b -> Int ->
                  Eff (random :: RANDOM | e) (CharTree a b)
insertAtOffset item tree@{items, allocType} 0 = case M.lookup 0 items of
  Just i -> do
    newSubtree <- insert' item Nil (Tuple 1 capacity) i.subtree
    pure $ CharTree $ tree {items = M.insert 0 (i {subtree = newSubtree}) items}
  Nothing -> do
    tree' <- CharTree <$> newCharTree >>= insert item Nil (Tuple 1 capacity)
    let newContainer = {id: Nothing, payload: Nothing, subtree: tree'}
    pure $ CharTree $ tree {items = M.insert 0 newContainer items}

insertAtOffset item tree@{items, allocType} idx = case M.lookup idx items of
  Just i -> do
    subtree <- insert item Nil (Tuple 1 capacity) i.subtree
    pure $ CharTree $ tree {items = M.insert idx (i {subtree = subtree}) items}
  Nothing -> pure $ CharTree $ tree {items = M.insert idx item items}


-- TODO: return a path  at which insertion took place
insert' :: forall a b e. Container a b -> List Int -> Tuple Int Int -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert' item (Cons x xs) coords Leaf = newCharTree >>= walkTree item coords x xs
insert' item (Cons x xs) coords (CharTree tree) = walkTree item coords x xs tree

insert' item Nil coords@(Tuple p q) t = case t of
  Leaf -> do
    tree <- newCharTree
    offset <- getOffset tree.allocType p q
    insertAtOffset item tree offset
  (CharTree tree@{items, allocType}) ->
    getOffset allocType p q >>= insertAtOffset item tree

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
