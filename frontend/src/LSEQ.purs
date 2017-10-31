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

import LSEQ.Types (CharTree(..), Letter, TreeBody, capacity)
import LSEQ.Helpers (getOffset, inBounds, newCharTree)


-- TODO
--
-- [ ] - move into separate modules
-- [ ] - move to foldable/traversable instances

walkTree :: forall a b e. Letter a b -> Tuple Int Int -> TreeBody a b -> Int -> List Int ->
            Eff (random :: RANDOM | e) (CharTree a b)
walkTree letter coords tree@{chars, allocType} idx xs =
  case M.lookup idx chars of
    Nothing   -> pure $ CharTree $ tree {chars = M.insert idx letter chars}
    Just char -> do
      subtree <- insert' letter xs coords char.subtree
      let char' = char {subtree = subtree}
      pure $ CharTree $ tree {chars = M.insert idx char' chars}

insert' :: forall a b e. Letter a b -> List Int -> Tuple Int Int -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert' letter _ _ Leaf = newCharTree >>= case _ of
  (CharTree tree@{chars, allocType}) -> do
    idx <- getOffset allocType 0 capacity
    pure $ CharTree $ tree {chars = M.insert idx letter chars}
  Leaf -> pure $ Leaf -- this should never happen lol

insert' letter Nil coords@(Tuple p q) (CharTree tree@{chars, allocType}) =
  getOffset allocType p q >>= \idx -> case idx == 0 && p == 0 && q == 0 of
    true -> case M.lookup idx chars of
            Just char -> do
              -- left side tree rotation
              -- insert the letter as a first element of the bottom subtree,
              -- with a leaf subtree
              let char'   = char {subtree = Leaf}
                  lowestIndex = case char.subtree of
                    Leaf             -> capacity
                    (CharTree tree') -> fromMaybe capacity $ _.key <$> M.findMin tree'.chars
                  insertBound = Tuple 0 (lowestIndex - 1)

              updatedTree <- insert char' Nil insertBound char.subtree
              let letter' = letter {subtree = updatedTree}

              pure $ CharTree $ tree {chars = M.insert idx letter' chars}
            Nothing -> walkTree letter coords tree idx Nil
    otherwise -> walkTree letter coords tree idx Nil

insert' letter (Cons x xs) coords (CharTree tree) = walkTree letter coords tree x xs

insert :: forall a b e. Letter a b -> List Int -> Tuple Int Int -> CharTree a b ->
          Eff (random :: RANDOM | e) (CharTree a b)
insert l path (Tuple p q) tree =
  insert' l path (Tuple (inBounds p) (inBounds q)) tree

delete :: forall a b. List Int -> Int -> CharTree a b -> CharTree a b
delete _ _ Leaf = Leaf
delete Nil p (CharTree tree@{chars}) = CharTree $ tree {chars = M.delete p chars}
delete (Cons x xs) p (CharTree tree@{chars}) = case letter of
    Nothing -> CharTree tree
    Just l  -> let withNewTree = l {subtree = delete xs p l.subtree} in
                  CharTree $ tree {chars = M.insert x withNewTree chars}
  where letter = M.lookup x chars
