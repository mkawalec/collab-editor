module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Components.TextContainer.Component as TC
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA
import Control.Monad.Eff.Random (RANDOM)

main :: Eff (HA.HalogenEffects (random :: RANDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI TC.component unit body
