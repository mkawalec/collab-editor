module LSEQ.Utility (
  print
, draw
, findPath
) where

import Prelude
import LSEQ.Types (CharTree(..), Letter)

import Data.List (List(..), foldMap)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.List as L
import Data.String as S
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))

import Data.Foldable (foldl)
import Data.Array (replicate)

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
