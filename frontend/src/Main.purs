module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Interface (ui)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
