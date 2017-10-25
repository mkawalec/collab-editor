module LSEQ where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool, random)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Int (toNumber)
import Data.List (List, head, tail)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Math (pow)

type Letter a = {
  letter :: Char
, meta :: a
, subtree :: CharTree a
}

data AllocType = Plus | Minus

data CharTree a = Leaf | CharTree {
  chars :: M.Map Int (Letter a)
, allocType :: AllocType
, capacity :: Int
}

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

view :: forall a. List Int -> CharTree a -> CharTree a
view _ Leaf    = Leaf
view path (CharTree {chars, allocType, capacity}) =
  case head path of
    Just pos -> case M.lookup pos chars of
      Just letter -> view (fromMaybe L.Nil $ tail path) letter.subtree
      Nothing     -> view L.Nil (CharTree {chars, allocType, capacity: capacity * 2})
    Nothing  -> (CharTree {chars, allocType, capacity})

modify :: forall a. List Int -> CharTree a -> CharTree a -> CharTree a
modify _ newSubtree Leaf    = newSubtree
modify path newSubtree (CharTree {chars, allocType, capacity}) = case head path of
  Just pos -> case M.lookup pos chars of
    Just letter -> let innerSubtree = modify (fromMaybe L.Nil $ tail path) newSubtree letter.subtree in
                      CharTree {
                        chars: M.insert pos (letter {subtree=innerSubtree}) chars
                      , allocType: allocType
                      , capacity: capacity}
    Nothing     -> newSubtree
  Nothing  -> newSubtree

newCharTree :: forall e a. Int -> Eff (random :: RANDOM | e) (CharTree a)
newCharTree capacity = do
    allocDirection <- randomBool
    let allocType = if allocDirection then Plus else Minus
    pure $ CharTree {
        chars: M.empty
      , allocType: allocType
      , capacity: capacity}

-- |A kumaraswamy distribution, chosen for a good look and a simple inverse CDF
kumaraswamy :: Number -> Number -> forall e. Eff (random :: RANDOM | e) Number
kumaraswamy a b =
  (\n -> pow (1.0 - pow (1.0 - n) (1.0 / b)) (1.0 / a)) <$> random

-- |Put at the random location between two positions, if there's no space,
--  descend down with an offset. The number of available nodes should be multiplied by two
insert :: forall a e. Letter a -> Position -> CharTree a -> Eff (random :: RANDOM | e) (CharTree a)
insert letter (Position subtree posA posB) (CharTree {chars, allocType, capacity}) = do
  randomNumber <- kumaraswamy 2.0 5.0
  let idx = posA + floor $ randomNumber * toNumber (posB - posA)
  case subtree' of
    CharTree {chars: chars', allocType: allocType', capacity: capacity'} -> do
      case M.lookup idx chars' of
        Nothing   -> CharTree {chars: M.insert idx letter chars', allocType: allocType', capacity: capacity'}
        Just char -> -- need to insert somewhere in that chars subtree
    Leaf ->

  where zoom     = view subtree tree
        subtree' = case zoom of
          Leaf -> unsafePerformEff $ newCharTree (capacity * 2)
          _    -> zoom
