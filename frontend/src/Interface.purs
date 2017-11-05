module Interface (
  ui
, Query
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Event.Event (Event, target)
import DOM.Node.Node (nodeValue)
import DOM.Node.Types (Node)
import DOM (DOM)
import Data.FastDiff (diff)
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Types (CharTree(..), Container,
  class CharTreeDisplay, displayElement)
import Data.LSEQ.Utility as LU
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Record.Builder (merge, build)
import Data.Op (Op(..))
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ

import Data.Newtype (wrap)

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

genDiffs :: String -> String -> List TreeOp
genDiffs a b = Nil --let diffs = diff a b

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
