module Data.LSEQ.Utility (
  print
, draw
, findPath
) where

import Prelude

import Ansi.Output (strikethrough)
import Control.Alt ((<|>))
import Data.Array (replicate)
import Data.Array as A
import Data.Foldable (foldl)
import Data.LSEQ.Types (CharTree(..), Container, class CharTreeDisplay, displayElement)
import Data.List (List(..), foldMap, (:))
import Data.List as L
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.String as S
import Data.Tuple (Tuple(..))

type Result' a b = {
  string :: Seq String
, containers :: Seq (Container a b)
, cache :: Map a (List Int)
}

type Result a b = {
  string :: String
, containers :: Array (Container a b)
, cache :: Map a (List Int)
}

-- no need to pass the map around
print' :: forall a b. Ord a => CharTreeDisplay b =>
                      List Int -> Map a (List Int) -> CharTree a b ->
                      Result' a b
print' path map Leaf = {string: Seq.empty, containers: Seq.empty, cache: map}
print' path m (CharTree {items}) = foldl walk acc asPairs
  where acc = {string: Seq.empty, containers: Seq.empty, cache: m}
        asPairs = (M.toAscUnfoldable items) :: Array (Tuple Int (Container a b))
        walk ({string: accS, containers: accC, cache: map}) (Tuple k v) =
          case (Tuple <$> v.payload <*> v.id) of
            Just (Tuple p id) ->
              let {string: s, containers: c, cache: map'} = print' (k:path) map v.subtree
                  s'  = accS `Seq.snoc` displayElement p `Seq.append` s
                  c' = accC `Seq.snoc` v `Seq.append` c
                  map'' = M.insert id (L.reverse $ k:path) map'
              in {string: s', containers: c', cache: map''}
            Nothing -> let {string: s, containers: c, cache: map'} =
                              print' (k:path) map v.subtree
                       in {
                          string: (accS `Seq.append` s)
                        , containers: (accC `Seq.append` c)
                        , cache: map'
                        }

print :: forall a b. Ord a => CharTreeDisplay b =>
                     CharTree a b -> Result a b
print tree = let {string: text, containers: containers, cache: pathMap} =
                    print' Nil M.empty tree
                 textAsString = S.joinWith "" $ Seq.toUnfoldable text
                 containersArray = Seq.toUnfoldable containers
             in {
                  string: textAsString
                , containers: containersArray
                , cache: pathMap
                }


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
