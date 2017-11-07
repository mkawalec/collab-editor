module Components.TextContainer.Types where

import Data.LSEQ.Types (class CharTreeDisplay, CharTree, Container)
import Data.Map (Map)
import Data.List (List)
import DOM.Node.Types (Node)
import Data.Tuple (Tuple)

newtype TreePayload = TreePayload {
  char :: String
}

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

type Changes = {
    ops :: List TreeOp
  , backend :: CharTree Int TreePayload
  , id :: Int
  }

instance displayPayload :: CharTreeDisplay TreePayload where
  displayElement (TreePayload {char}) = char
