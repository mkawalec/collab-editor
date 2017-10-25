module LSEQ where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool, random)
import Data.Int (floor, toNumber)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (pow)

type Letter a = {
  letter :: Char
, meta :: a
, subtree :: CharTree a
}

data AllocType = Plus | Minus

type TreeBody a = {
  chars :: M.Map Int (Letter a)
, allocType :: AllocType
}

data CharTree a = Leaf | CharTree (TreeBody a)

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

newCharTree :: forall e a. Eff (random :: RANDOM | e) (CharTree a)
newCharTree = do
    allocDirection <- randomBool
    let allocType = if allocDirection then Plus else Minus
    pure $ CharTree {
        chars: M.empty
      , allocType: allocType}

-- |A kumaraswamy distribution, chosen for a good look and a simple inverse CDF
kumaraswamy :: Number -> Number -> forall e. Eff (random :: RANDOM | e) Number
kumaraswamy a b =
  (\n -> pow (1.0 - pow (1.0 - n) (1.0 / b)) (1.0 / a)) <$> random

capacity :: Int
capacity = 50

getOffset :: forall e. AllocType -> Int -> Int -> Eff (random :: RANDOM | e) Int
getOffset allocType p q = do
    n <- kumaraswamy 2.0 5.0
    let offset = floor $ n * (toNumber (q - p))
        idx    = case allocType of
                  Plus  -> p + offset
                  Minus -> q - offset
    pure idx

walkTree :: forall a e. Letter a -> Int -> Tuple Int Int -> TreeBody a -> Eff (random :: RANDOM | e) (CharTree a)
walkTree letter idx coords tree@{chars, allocType} =
  case M.lookup idx chars of
    Nothing   -> pure $ CharTree $ tree {chars = M.insert idx letter chars}
    Just char -> do
      subtree <- insert letter Nil coords char.subtree
      let char' = char {subtree = subtree}
      pure $ CharTree $ tree {chars = M.insert idx char' chars}

insert :: forall a e. Letter a -> List Int -> Tuple Int Int -> CharTree a -> Eff (random :: RANDOM | e) (CharTree a)
insert letter _ _ Leaf = newCharTree >>= case _ of
  (CharTree tree@{chars, allocType}) -> do
    idx <- getOffset allocType 0 capacity
    pure $ CharTree $ tree {chars = M.insert idx letter chars}
  Leaf -> pure $ Leaf -- this should never happen lol

insert letter Nil coords@(Tuple p q) (CharTree tree@{chars, allocType}) = do
  idx <- getOffset allocType p q
  walkTree letter idx coords tree

insert letter (Cons x xs) coords (CharTree tree) = walkTree letter x coords tree
