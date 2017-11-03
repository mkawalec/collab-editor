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

import LSEQ.Types (CharTree(..), Container, TreeBody, Position(..))
import LSEQ.Helpers (getOffset, newCharTree)


walkTree :: forall a b e. Container a b -> Tuple Position Position ->
            Int -> List Int -> Int -> TreeBody a b ->
            Eff (random :: RANDOM | e) (CharTree a b)
walkTree item coords idx xs c tree@{items, allocType} =
  case M.lookup idx items of
    Nothing   -> do --pure $ CharTree $ tree {items = M.insert idx item items}
      subtree <- CharTree <$> newCharTree >>= insert' item xs coords (2 * c)
      let container = {id: Nothing, payload: Nothing, subtree: subtree}
      pure $ CharTree $ tree {items = M.insert idx container items}
    Just char -> do
      subtree <- insert' item xs coords (2 * c) char.subtree
      let char' = char {subtree = subtree}
      pure $ CharTree $ tree {items = M.insert idx char' items}

insertAtOffset :: forall a b e. Container a b -> Int -> TreeBody a b -> Int ->
                  Eff (random :: RANDOM | e) (CharTree a b)
insertAtOffset item c tree@{items, allocType} 0 = case M.lookup 0 items of
  Just i -> do
    newSubtree <- insert' item Nil (Tuple (N 1) End) (2 * c) i.subtree
    pure $ CharTree $ tree {items = M.insert 0 (i {subtree = newSubtree}) items}
  Nothing -> do
    tree' <- CharTree <$> newCharTree >>= insert' item Nil (Tuple (N 1) End) (2 * c)
    let newContainer = {id: Nothing, payload: Nothing, subtree: tree'}
    pure $ CharTree $ tree {items = M.insert 0 newContainer items}

insertAtOffset item c tree@{items, allocType} idx = case M.lookup idx items of
  Just i -> do
    subtree <- insert' item Nil (Tuple (N 1) End) (2 * c) i.subtree
    pure $ CharTree $ tree {items = M.insert idx (i {subtree = subtree}) items}
  Nothing -> pure $ CharTree $ tree {items = M.insert idx item items}


-- TODO: return a path  at which insertion took place
insert' :: forall a b e. Container a b -> List Int ->
           Tuple Position Position -> Int -> CharTree a b ->
           Eff (random :: RANDOM | e) (CharTree a b)
insert' item (Cons x xs) coords c Leaf = newCharTree >>= walkTree item coords x xs (2 * c)
insert' item (Cons x xs) coords c (CharTree tree) = walkTree item coords x xs (2 * c) tree

insert' item Nil coords@(Tuple p q) c t = case t of
  Leaf -> do
    tree <- newCharTree
    offset <- getOffset tree.allocType c p q
    insertAtOffset item c tree offset
  (CharTree tree@{items, allocType}) ->
    getOffset allocType c p q >>= insertAtOffset item c tree

insert :: forall a b e. Container a b -> List Int -> Tuple Position Position -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert item path coords tree = insert' item path coords 30 tree

delete :: forall a b. List Int -> Int -> CharTree a b -> CharTree a b
delete _ _ Leaf = Leaf
delete Nil p (CharTree tree@{items}) = CharTree $ tree {items = M.delete p items}
delete (Cons x xs) p (CharTree tree@{items}) = case item of
    Nothing -> CharTree tree
    Just l  -> let withNewTree = l {subtree = delete xs p l.subtree} in
                  CharTree $ tree {items = M.insert x withNewTree items}
  where item = M.lookup x items
