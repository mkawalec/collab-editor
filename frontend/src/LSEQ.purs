module LSEQ where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool, random)
import Data.Array (replicate)
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Int.Bits (xor)
import Data.List (List(..), foldMap)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Debug.Trace (trace, traceShow)
import Math (pow)

type Letter a b = {
  letter :: Char
, meta :: a
, id :: b
, subtree :: CharTree a b
}

data AllocType = Plus | Minus

type TreeBody a b = {
  chars :: M.Map Int (Letter a b)
, allocType :: AllocType
}

data CharTree a b = Leaf | CharTree (TreeBody a b)

data Position = Position (List Int) Int Int -- | (Subtree, p, q)

newCharTree :: forall e a b. Eff (random :: RANDOM | e) (CharTree a b)
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

getOffset :: forall e. AllocType -> Int -> Int ->
             Eff (random :: RANDOM | e) Int
getOffset allocType p q = let p' = inBounds p
                              q' = inBounds q
                          in
  do
    n <- kumaraswamy 2.0 5.0
    let offset = floor $ n * (toNumber (q' - p'))
        idx    = case allocType of
                  Plus  -> p' + offset
                  Minus -> q' - offset
    pure idx

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
insert l path (Tuple p q) tree = let p' = inBounds p
                                     q' = inBounds q
                                 in insert' l path (Tuple p' q') tree

inBounds :: Int -> Int
inBounds x = let lower = if x < 0 then 0 else x
             in if lower > capacity then capacity else lower

delete :: forall a b. List Int -> Int -> CharTree a b -> CharTree a b
delete _ _ Leaf = Leaf
delete Nil p (CharTree tree@{chars}) = CharTree $ tree {chars = M.delete p chars}
delete (Cons x xs) p (CharTree tree@{chars}) = case letter of
    Nothing -> CharTree tree
    Just l  -> let withNewTree = l {subtree = delete xs p l.subtree} in
                  CharTree $ tree {chars = M.insert x withNewTree chars}
  where letter = M.lookup x chars

print :: forall a b. CharTree a b -> String
print Leaf = ""
print (CharTree {chars}) = foldMap (\l -> S.singleton l.letter <> print l.subtree) ch
    where ch = M.values chars

findPath' :: forall a b. Eq b => List Int -> b -> CharTree a b -> Maybe (List Int)
findPath' _ _ Leaf = Nothing
findPath' path id (CharTree {chars}) =
  let ch = M.toAscUnfoldable chars
      walker = (\acc (Tuple k v) ->
        if v.id == id
          then Just (Cons k path)
          else acc <|> findPath' (Cons k path) id v.subtree)
    in
foldl walker Nothing (ch :: List (Tuple Int (Letter a b)))

findPath :: forall a b. Eq b => b -> CharTree a b -> Maybe (Tuple (List Int) Int)
findPath id tree = case L.reverse <$> findPath' Nil id tree of
  Nothing   -> Nothing
  Just path -> L.init path >>= (\p -> (Tuple p) <$> L.last path)


draw :: forall a b. Show b => Int -> CharTree a b -> String
draw _ Leaf = ""
draw indent (CharTree {chars}) =
    foldl (\acc (Tuple k v) -> acc <> indentStr <> "|- " <> S.singleton v.letter <> ", " <> show v.id <> " at idx " <> show k <> "\n" <> draw (indent + 1) v.subtree) "" values
  where values = M.toAscUnfoldable chars :: List (Tuple Int (Letter a b))
        indentStr = foldl (\s p -> s <> p) "" $ replicate indent "  "
