module LSEQ (
  insert
, insert'
, delete
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), snd)

import LSEQ.Types (CharTree(..), Container, TreeBody, Position(..))
import LSEQ.Helpers (getOffset, newCharTree)

type Path = List Int

-- |Walks through a tree, creating nodes as neccessary and noting the path
--  that has been walked
walkTree :: forall a b e. Container a b -> Tuple Position Position ->
            Int -> Path -> Int -> Path -> TreeBody a b ->
            Eff (random :: RANDOM | e) (Tuple (CharTree a b) Path)
walkTree item coords idx xs c path tree@{items, allocType} =
  case M.lookup idx items of
    Nothing   -> do
      (Tuple subtree insertPath) <-
        CharTree <$> newCharTree >>= insert' item xs coords (2 * c) (idx:path)
      let container = {id: Nothing, payload: Nothing, subtree: subtree}
      pure $ Tuple (CharTree $ tree {items = M.insert idx container items})
                   insertPath
    Just char -> do
      (Tuple subtree insertPath) <-
        insert' item xs coords (2 * c) (idx:path) char.subtree
      let char' = char {subtree = subtree}
      pure $ Tuple (CharTree $ tree {items = M.insert idx char' items})
                   insertPath

insertAtOffset :: forall a b e. Container a b -> Int ->
                  TreeBody a b -> Path -> Int ->
                  Eff (random :: RANDOM | e) (Tuple (CharTree a b) Path)
insertAtOffset item c tree@{items, allocType} path 0 = case M.lookup 0 items of
  Just i -> do
    (Tuple newSubtree insertPath) <-
      insert' item Nil (Tuple (N 1) End) (2 * c) (0:path) i.subtree
    pure $ Tuple (CharTree $ tree {items = M.insert 0 (i {subtree = newSubtree}) items})
                 insertPath
  Nothing -> do
    (Tuple tree' insertPath) <-
      CharTree <$> newCharTree >>= insert' item Nil (Tuple (N 1) End) (2 * c) (0:path)
    let newContainer = {id: Nothing, payload: Nothing, subtree: tree'}
    pure $ Tuple (CharTree $ tree {items = M.insert 0 newContainer items})
                 insertPath

insertAtOffset item c tree@{items, allocType} path idx = case M.lookup idx items of
  Just i -> do
    (Tuple subtree insertPath) <-
      insert' item Nil (Tuple (N 1) End) (2 * c) (idx:path) i.subtree
    pure $ Tuple (CharTree $ tree {items = M.insert idx (i {subtree = subtree}) items})
           insertPath
  Nothing -> pure $ Tuple (CharTree $ tree {items = M.insert idx item items})
                          (idx:path)

insert' :: forall a b e. Container a b -> Path ->
           Tuple Position Position -> Int -> Path -> CharTree a b ->
           Eff (random :: RANDOM | e) (Tuple (CharTree a b) Path)
insert' item (Cons x xs) coords c pathWalked Leaf =
  newCharTree >>= walkTree item coords x xs (2 * c) pathWalked
insert' item (Cons x xs) coords c pathWalked (CharTree tree) =
  walkTree item coords x xs (2 * c) pathWalked tree

insert' item Nil coords@(Tuple p q) c pathWalked t = case t of
  Leaf -> do
    tree <- newCharTree
    offset <- getOffset tree.allocType c p q
    insertAtOffset item c tree pathWalked offset
  (CharTree tree@{items, allocType}) ->
    getOffset allocType c p q >>= insertAtOffset item c tree pathWalked

insert :: forall a b e. Container a b -> Path ->
          Tuple Position Position -> CharTree a b ->
          Eff (random :: RANDOM | e) (Tuple (CharTree a b) Path)
insert item path coords tree =
  bimap id L.reverse <$> insert' item path coords 40 Nil tree

isSubtreeEmpty :: forall a b. CharTree a b -> Boolean
isSubtreeEmpty Leaf               = true
isSubtreeEmpty (CharTree {items}) =
  let containers = (map snd $ M.toUnfoldable items) :: List (Container a b)
      walker acc i = acc || isJust i.id || isJust i.payload || isSubtreeEmpty i.subtree
  in not $ L.foldl walker false containers


-- TODO: This is super borken, let's make it work :P
delete :: forall a b. Path -> Int -> CharTree a b -> CharTree a b
delete _ _ Leaf = Leaf
delete Nil p (CharTree tree@{items}) = case M.lookup p items of
  Nothing -> CharTree tree
  Just item -> case isSubtreeEmpty item.subtree of
    true -> CharTree $ tree {items = M.delete p items}
    false -> let emptyContainer = item {id = Nothing, payload = Nothing } in
             CharTree $ tree {items = M.insert p emptyContainer items}
delete (Cons x xs) p (CharTree tree@{items}) = case item of
    Nothing -> CharTree tree
    Just l  -> let withNewTree = l {subtree = delete xs p l.subtree} in
                  CharTree $ tree {items = M.insert x withNewTree items}
  where item = M.lookup x items
