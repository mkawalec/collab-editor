module LSEQ.Utility (
  print
, draw
, findPath
) where

import Prelude
import LSEQ.Types (CharTree(..), Container,
  class CharTreeDisplay, displayElement)

import Data.List (List(..), foldMap)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Alt ((<|>))

import Data.Foldable (foldl)
import Data.Array (replicate)

print :: forall a b. CharTreeDisplay b => CharTree a b -> String
print Leaf = ""
print (CharTree {items}) = let containers = M.values items in
  foldMap (\l -> case l.payload of
    Just p -> displayElement p <> print l.subtree
    Nothing -> print l.subtree
    ) containers

findPath' :: forall a b. Eq a => List Int -> Maybe a -> CharTree a b -> Maybe (List Int)
findPath' _ _ Leaf = Nothing
findPath' path id (CharTree {items}) =
  let ch = M.toAscUnfoldable items
      walker = (\acc (Tuple k v) ->
        if v.id == id
          then Just (Cons k path)
          else acc <|> findPath' (Cons k path) id v.subtree)
    in
foldl walker Nothing (ch :: List (Tuple Int (Container a b)))

findPath :: forall a b. Eq a => Maybe a -> CharTree a b -> Maybe (Tuple (List Int) Int)
findPath id tree = case L.reverse <$> findPath' Nil id tree of
  Nothing   -> Nothing
  Just path -> L.init path >>= (\p -> (Tuple p) <$> L.last path)

draw :: forall a b. Show a => CharTreeDisplay b => Int -> CharTree a b -> String
draw _ Leaf = ""
draw indent (CharTree {items}) =
    foldl (\acc (Tuple k v) -> acc <>
      indentStr <> "|- " <> fromMaybe "Nil" (displayElement <$> v.payload) <> ", " <>
      show v.id <> " at idx " <> show k <> "\n" <>
      draw (indent + 1) v.subtree) "" values
  where values = M.toAscUnfoldable items :: List (Tuple Int (Container a b))
        indentStr = foldl (\s p -> s <> p) "" $ replicate indent "  "
