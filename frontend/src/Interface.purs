module Interface (
  ui
, Query
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Event.Event (Event, target)
import DOM.Node.Node (nodeValue)
import DOM.Node.Types (Node)
import Data.FastDiff (diff)
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Types (CharTree(..), class CharTreeDisplay, displayElement)
import Data.LSEQ.Utility as LU
import Data.Maybe (Maybe(..))
import Data.Op (Op(..))
import Data.String as S
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
}

data Query a = DoNothing a | UpdateText Node a

data Op = Insert (List Int) (Tuple Int Int) TreePayload | Delete (List Int)

genDiffs :: String -> String -> List Op
genDiffs a b = let diffs = diff a b

ui :: forall e. H.Component HH.HTML Query Unit Void (Aff e)
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {dataBackend: CharTree $ unsafePerformEff newCharTree}

  textAreaChanged :: Event -> HQ.Action Query
  textAreaChangedg = UpdateText <<< target

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.textarea [
          HE.onInput (HE.input textAreaChanged)
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff e)
  eval = case _ of
    DoNothing next -> pure next
    UpdateText node next -> do
      nodeText <- liftEff $ nodeValue node
      backend <- H.gets _.dataBackend

      let currentText = LU.print backend

      pure next
