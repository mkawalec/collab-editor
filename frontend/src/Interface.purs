module Interface (
  ui
, Query
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (Event, target)
import DOM.Node.Types (Node)
import Data.Array ((!!))
import Data.FastDiff (diff, OpType(..))
import Data.Foldable (foldM, foldl)
import Data.DOMHelpers as DH
import Data.LSEQ as LS
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Types (class CharTreeDisplay, CharTree(..), Container, Position(..), displayElement)
import Data.LSEQ.Utility as LU
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Op (Op(..))
import Data.Record as R
import Data.Record.Builder (merge, build)
import Data.String as S
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Debug.Trace as DT

newtype TreePayload = TreePayload {
  char :: String
}

instance displayPayload :: CharTreeDisplay TreePayload where
  displayElement (TreePayload {char}) = char

type State = {
  backend :: CharTree Int TreePayload
, string :: String
, containers :: Array (Container Int TreePayload)
, cache :: Map Int (List Int)
, currentId :: Int
}

data Query a = DoNothing a | UpdateText Node a

data TreeOp = OpInsert (List Int) TreePayload | OpDelete (Tuple (List Int) Int)

-- TODO:
-- [ ] Simplify this stuff A LOT to make it readable
-- [x] Use intervals, not unform on the whole range

type GenDiffResult = {
    ops :: List TreeOp
  , backend :: CharTree Int TreePayload
  , id :: Int
  }
genDiffs :: forall e. State -> String -> String ->
            Eff (random :: RANDOM | e) GenDiffResult
genDiffs state a b = let diffs = diff a b in do
    {idx, ops, state} <- DT.traceShow diffs \_ -> foldM processDiff {idx: 0, ops: Nil, state} diffs
    pure {ops: ops, backend: state.backend, id: state.currentId}


type DiffState = {idx :: Int, ops :: List TreeOp, state :: State}
processDiff :: forall e. DiffState -> (Tuple OpType String) ->
               Eff (random :: RANDOM | e) DiffState
processDiff state@({idx}) (Tuple Equal diff) = pure state { idx = idx + S.length diff }
processDiff diffState (Tuple Delete diff) =
    pure $ foldl deleteLetter diffState letters
  where letters = S.split (S.Pattern "") diff
        deleteLetter {idx, ops, state} l = let nodePos = lookupIdx idx state in
          DT.trace ("delete " <> show l <> " pos " <> show idx <> " nodePos " <> show nodePos) \_ -> {
            idx: idx + 1
          , ops: (OpDelete nodePos):ops
          , state: state { backend = uncurry LS.delete nodePos state.backend}
          }

processDiff ({idx, ops, state}) (Tuple Insert diff) = do
    {idx, ops, state} <- foldM walker {
      prev: Nothing
      , idx: idx
      , ops: ops
      , state: state
    } letters
    pure $ {idx, ops, state}
  where letters = S.split (S.Pattern "") diff
        walker {prev, idx, ops, state} char = case prev of
          Nothing -> let (Tuple path p) = lookupPrevIdx idx state
                         atPath = DT.trace ("p " <> show p <> " path " <> show path) \_ -> zoom path state.backend
                         q = case atPath of
                          Leaf -> End
                          (CharTree tree) -> case M.lookupGT p tree.items of
                            Nothing -> End
                            Just {key, value} -> N (key - 1)
                         payload = DT.trace ("q " <> show q) \_ -> {
                             id: Just $ state.currentId + 1
                           , payload: Just $ TreePayload {char}
                           , subtree: Leaf
                         }
                      in do
                      (Tuple tree path') <- LS.insert payload path (Tuple (N p) q) state.backend
                      pure {
                          idx: idx + 1
                        , prev: L.unsnoc path'
                        , ops: (OpInsert path' $ TreePayload {char}):ops
                        , state: state {backend = tree, currentId = state.currentId + 1}
                      }
          Just ({init, last}) ->
            let atPath = zoom init state.backend
                q = case atPath of
                  Leaf -> End
                  (CharTree tree) -> case M.lookupGT last tree.items of
                    Nothing -> End
                    Just {key, value} -> N (key - 1)
                payload = {
                    id: Just $ state.currentId + 1
                  , payload: Just $ TreePayload {char}
                  , subtree: Leaf
                }
            in do
            (Tuple tree path) <- LS.insert payload init (Tuple (N last) q) state.backend
            pure {
                idx: idx + 1
              , prev: L.unsnoc path
              , ops: (OpInsert path $ TreePayload {char}):ops
              , state: state {backend = tree, currentId = state.currentId + 1}
            }
            --pure (L.unsnoc path >>= Tuple tree')



-- TODO: This should be a lens
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
  where result = state.containers !! i >>= \c ->
                 M.lookup (fromMaybe 0 c.id) state.cache >>= \path ->
                 L.unsnoc path >>= \{init, last} ->
                 Just (Tuple init last)

ui :: forall e. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM, random :: RANDOM | e))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = build (merge {backend: tree, currentId: 1}) (LU.print tree)
    where tree = CharTree $ unsafePerformEff newCharTree

  textAreaChanged :: Event -> HQ.Action Query
  textAreaChanged = UpdateText <<< target

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.textarea [
          HE.onInput (HE.input textAreaChanged)
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, random :: RANDOM | e))
  eval = case _ of
    DoNothing next -> pure next
    UpdateText node next -> do
      nodeText <- H.liftEff $ DH.value node
      state <- H.get

      {ops, backend, id} <- DT.trace ("text " <> nodeText) \_ -> liftEff $ genDiffs state state.string nodeText

      -- Actually apply the new value
      let {string, containers, cache} = LU.print backend
      DT.trace (LU.draw 0 backend) \_ -> H.liftEff $ DH.setValue string node

      H.put {
          backend
        , string
        , containers
        , cache
        , currentId: id
      }

      pure next
