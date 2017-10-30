module Test.Main where

import LSEQ
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.State (evalState, State(..), put, get)
import Data.Array as A
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit(..))
import LSEQ as L
import Test.QuickCheck ((===), (/==), Result(..))
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

makeLetter :: Char -> Int -> Letter Unit Int
makeLetter l id = {letter: l, id: id, meta: unit, subtree: Leaf}

main :: Eff (RunnerEffects (QCRunnerEffects (random :: RANDOM))) Unit
main = run [consoleReporter] do
  describe "LSEQ" do
    it "inserts a single character" do

      emptyTree <- liftEff $ newCharTree
      withA <- liftEff $ insert (makeLetter 'a' 0) Nil (Tuple 1 1) emptyTree
      L.print withA `shouldEqual` "a"

    it "inserts arbitrary strings character by character" do
      emptyTree <- liftEff $ newCharTree

      quickCheck \str -> do
        let chars = A.zip (S.toCharArray str) (A.range 0 $ S.length str)
            letters = map (\(Tuple c id) -> makeLetter c id) chars
            charsWithPrevious = [Nothing] <> map Just letters
            result = foldl (
              \tree (Tuple l prev) -> case prev of
                Nothing -> unsafePerformEff $ insert l Nil (Tuple 0 capacity) tree
                Just l' -> case findPath l'.id tree of
                  Nothing -> tree
                  -- we need to define a lens to zoom in on this fragment
                  Just (Tuple path id) -> unsafePerformEff $ insert l path (Tuple id (id + 1)) tree
              ) emptyTree $ A.zip letters charsWithPrevious

        L.print result === str
