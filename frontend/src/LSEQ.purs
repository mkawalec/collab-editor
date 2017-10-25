module LSEQ where

import Control.Monad.Eff.Random (RANDOM, randomBool)
import Control.Monad.Eff (Eff)
import Data.List (List, head, tail)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

type Letter a = {
  letter :: Char
, meta :: a
, subtree :: CharTree a
}

data AllocType = Plus | Minus

data CharTree a = Leaf | CharTree {
  chars :: M.Map Int (Letter a)
, allocType :: AllocType
}

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

empty :: forall a. AllocType -> CharTree a
empty alloc = CharTree {chars: M.empty, allocType: alloc}

view :: forall a. List Int -> CharTree a -> CharTree a
view _ Leaf    = Leaf
view path (CharTree {chars, allocType}) =
  case head path of
    Just pos -> case M.lookup pos chars of
      Just letter -> view (fromMaybe L.Nil $ tail path) letter.subtree
      Nothing     -> view L.Nil (CharTree {chars, allocType})
    Nothing  -> (CharTree {chars, allocType})

modify :: forall a. List Int -> CharTree a -> CharTree a -> CharTree a
modify _ newSubtree Leaf    = newSubtree
modify path newSubtree (CharTree {chars, allocType}) = case head path of
  Just pos -> case M.lookup pos chars of
    Just letter -> let innerSubtree = modify (fromMaybe L.Nil $ tail path) newSubtree letter.subtree in
                      CharTree {chars: M.insert pos (letter {subtree=innerSubtree}) chars, allocType: allocType}
    Nothing     -> newSubtree
  Nothing  -> newSubtree

newCharTree :: forall e a. Eff (random :: RANDOM | e) (CharTree a)
newCharTree = do
    allocDirection <- randomBool
    let allocType = if allocDirection then Plus else Minus
    pure $ empty allocType

-- |Put at the random location between two positions, if there's no space,
--  descend down with an offset. The number of available nodes should be multiplied by two
insert :: forall a. Letter a -> Position -> CharTree a -> CharTree a
insert letter (Position subtree posA posB) tree = tree
  where zoom = view subtree tree
        sub = case zoom of
          Leaf -> unsafePerformEff newCharTree
          _    -> zoom
