module Components.TextContainer.Transition (
  transition
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.FastDiff (diff, OpType(..))
import Data.Foldable (foldM, foldl)
import Data.Array ((!!))
import Data.LSEQ as LS
import Data.LSEQ.Types (CharTree(..), Position(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..), uncurry)

import Components.TextContainer.Types (Changes, State,
  TreeOp(..), TreePayload(..))

transition :: forall e. State -> String -> String ->
            Eff (random :: RANDOM | e) Changes
transition state a b = let diffs = diff a b in do
    {idx, ops, state: state'} <- foldM processDiff {idx: 0, ops: Nil, state} diffs
    pure {ops: ops, backend: state'.backend, id: state'.currentId}


type DiffState = {idx :: Int, ops :: List TreeOp, state :: State}
processDiff :: forall e. DiffState -> (Tuple OpType String) ->
               Eff (random :: RANDOM | e) DiffState
processDiff state@({idx}) (Tuple Equal diff) = pure state { idx = idx + S.length diff }
processDiff diffState (Tuple Delete diff) =
    pure $ foldl deleteLetter diffState letters
  where letters = S.split (S.Pattern "") diff
        deleteLetter {idx, ops, state} l = let nodePos = lookupIdx idx state in
          {
            idx: idx + 1
          , ops: (OpDelete nodePos):ops
          , state: state { backend = uncurry LS.delete nodePos state.backend}
          }

processDiff ({idx, ops, state}) (Tuple Insert diff) = do
    {idx: idx', ops: ops', state: state'} <- foldM walker {
        prev: Nothing
      , idx
      , ops
      , state
    } letters
    pure $ {idx: idx', ops: ops', state: state'}
  where letters = S.split (S.Pattern "") diff
        upperBound tree path p = case zoom path tree of
          Leaf -> End
          (CharTree tree') -> case M.lookupGT p tree'.items of
            Nothing -> End
            Just {key, value} -> N (key - 1)
        walker {prev, idx: idx', ops: ops', state: state'} char =
          let (Tuple path p) = case prev of
                Nothing -> lookupPrevIdx idx' state'
                Just ({init, last}) -> Tuple init last
              q = upperBound state'.backend path p
              payload = {
                  id: Just $ state'.currentId + 1
                , payload: Just $ TreePayload {char}
                , subtree: Leaf
              }
          in do
            (Tuple tree path') <- LS.insert payload path (Tuple (N p) q) state'.backend
            pure {
              idx: idx' + 1
              , prev: L.unsnoc path'
              , ops: (OpInsert path' $ TreePayload {char}):ops
              , state: state' {backend = tree, currentId = state'.currentId + 1}
              }

zoom :: forall a b. (List Int) -> CharTree a b -> CharTree a b
zoom Nil tree = tree
zoom _ Leaf = Leaf
zoom (Cons x xs) (CharTree tree) = case M.lookup x tree.items of
  Nothing -> Leaf
  Just container -> zoom xs container.subtree

lookupPrevIdx :: Int -> State -> Tuple (List Int) Int
lookupPrevIdx 0 state = let (Tuple path _) = lookupIdx 0 state in Tuple path 0
lookupPrevIdx i state = lookupIdx (i - 1) state

lookupIdx :: Int -> State -> Tuple (List Int) Int
lookupIdx i state = case result of
    Nothing -> Tuple Nil 1
    Just r  -> r
  where result =  state.containers !! i >>= \c ->
                 M.lookup (fromMaybe 0 c.id) state.cache >>= \path ->
                 L.unsnoc path >>= \{init, last} ->
                 Just (Tuple init last)
