module LSEQ.Helpers where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool, random)
import Data.Map as M

import Math (pow)
import Data.Int (floor, toNumber)

import LSEQ.Types (AllocType(..), TreeBody, capacity)


newCharTree :: forall e a b. Eff (random :: RANDOM | e) (TreeBody a b)
newCharTree = do
    allocDirection <- randomBool
    let allocType = if allocDirection then Plus else Minus
    pure $ {
        items: M.empty
      , allocType: allocType}

-- |A kumaraswamy distribution, chosen for a good look and a simple inverse CDF
kumaraswamy :: Number -> Number -> forall e. Eff (random :: RANDOM | e) Number
kumaraswamy a b =
  (\n -> pow (1.0 - pow (1.0 - n) (1.0 / b)) (1.0 / a)) <$> random

getOffset :: forall e. AllocType -> Int -> Int ->
             Eff (random :: RANDOM | e) Int
getOffset allocType p q = let p' = inBounds p
                              q' = inBounds q
                          in
  do
    n <- kumaraswamy 2.0 5.0
    let offset = floor $ n * (toNumber (q' - p'))
        idx    = case allocType of
                  Plus  -> p' + offset
                  Minus -> q' - offset
    pure idx

inBounds :: Int -> Int
inBounds x = let lower = if x < 0 then 0 else x
             in if lower > capacity then capacity else lower
