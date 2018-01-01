module Components.TextContainer.Component (
  component
) where

import Prelude

import DOM (DOM)
import DOM.Event.Event (Event, target)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import CSS as CSS
import CSS.Size (px, pct, vh)
import Halogen.Query as HQ

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Class (liftEff)
import Data.Record.Builder (merge, build)
import Data.DOMHelpers as DH
import Data.Maybe (Maybe(..))

import Data.LSEQ.Types (CharTree(..))
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Utility as LU

import Components.TextContainer.Types (Query(..), State)
import Components.TextContainer.Transition (transition)

type Effects e =  Aff (dom :: DOM, random :: RANDOM | e)

-- TODO:
-- [ ] Make a PR to DOM with DOMHelpers

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component =
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
    HH.div [ style do CSS.width $ px 500.0
                      CSS.height $ vh 100.0 ]
      [ HH.textarea [
          HE.onInput (HE.input textAreaChanged)
        , HP.value st.string
        , style do CSS.width $ pct 100.0
                   CSS.height $ pct 100.0
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Effects e)
  eval = case _ of
    DoNothing next -> pure next
    UpdateText node next -> do
      nodeText <- H.liftEff $ DH.value node
      state <- H.get

      {ops, backend, id} <- liftEff $ transition state state.string nodeText
      let {string, containers, cache} = LU.print backend

      H.put {
          backend
        , string
        , containers
        , cache
        , currentId: id
      }

      pure next
