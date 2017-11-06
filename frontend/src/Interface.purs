module Interface (
  ui
, Query
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import DOM.Event.Event (Event, target)
import DOM.Node.Node (nodeValue)
import DOM.Node.Types (Node)
import Data.FastDiff (diff)
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Types (class CharTreeDisplay, CharTree(..), Container, Position(..), displayElement)
import Data.LSEQ.Utility as LU
import Data.List (List(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Op (Op(..))
import Data.Record.Builder (merge, build)
import Data.String as S
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ

newtype TreePayload = TreePayload {
  char :: Char
}

instance displayPayload :: CharTreeDisplay TreePayload where
  displayElement (TreePayload {char}) = S.singleton char

type State = {
  dataBackend :: CharTree Int TreePayload
, string :: String
, containers :: Array (Container Int TreePayload)
, cache :: Map Int (List Int)
}

data Query a = DoNothing a | UpdateText Node a

data TreeOp = Insert (List Int) (Tuple Int Int) TreePayload | Delete (List Int)

genDiffs :: State -> String -> String -> Tuple (List TreeOp) (CharTree Int TreePayload)
genDiffs state a b =
  where diffs = diff a b


type DiffState = {idx :: Int, ops :: List TreeOp, state :: State}
processDiff :: DiffState -> (Tuple OpType String) ->
               Eff (random :: RANDOM | e) DiffState
processDiff ({idx, ops, state}) (Tuple Equal diff) = {
    idx: (idx + S.length diff)
  , ops
  , state
  }

processDiff ({idx, ops, state}) (Tuple Insert diff) = foldl
  where letters = S.splitAt (Pattern "") diff
        walker {idx, ops, state, prev} char = case prev of
          Nothing -> let (Tuple path p) = lookupIdx idx state
                         atPath = zoom path state.dataBackend
                         q = case p of
                          End -> End
                          N idx -> case atPath of
                            Leaf -> End
                            (CharTree tree) -> case M.lookupGT idx tree.items of
                              Nothing -> End
                              Just {key, value} -> N key
                      in
                      L.insert (TreePayload {char: char}) path (Tuple p q) state.dataBackend


-- TODO: This should be a lens
zoom :: forall a b. (List Int) -> CharTree a b -> CharTree a b
zoom Nil tree = tree
zoom _ Leaf = Leaf
zoom (Cons x xs) (CharTree tree) = case M.lookup x tree.items of
  Nothing -> Leaf
  Just container -> zoom xs container.subtree

lookupIdx :: Int -> State -> Tuple (List Int) Position
lookupIdx 0 _ = Tuple Nil (N 0)
lookupIdx i state = case result of
    Nothing -> Tuple Nil End
    Just r  -> Just r
  where result = state.containers !! (i - 1) >>= \c ->
                 M.lookup (fromMaybe 0 c.id) state.cache >>= \path ->
                 L.unsnoc path >>= \{init, last} ->
                 Just (Tuple init (N last))

-- TODO: Quickly generate a set of diffs to apply to a tree,
--       apply them, generate a new output


-- We need: print that returns raw nodes too

ui :: forall e. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | e))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = build (merge {dataBackend: tree}) (LU.print tree)
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | e))
  eval = case _ of
    DoNothing next -> pure next
    UpdateText node next -> do
      nodeText <- H.liftEff $ nodeValue node
      backend <- H.gets _.dataBackend

      let currentText = LU.print backend

      pure next
