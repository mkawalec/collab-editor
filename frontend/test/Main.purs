module Test.Main where

import LSEQ
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.State (evalState, State(..), put, get)
import Data.Array as A
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List.Lazy (stripPrefix)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Traversable (traverse, scanl)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit(..))
import Debug.Trace (trace, traceShow)
import LSEQ as L
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((===), (/==), Result(..))
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Control.Monad (whenM)
import Data.Maybe (fromMaybe)

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
                  Nothing -> trace "Didn't find a path" \_ -> tree
                  -- we need to define a lens to zoom in on this fragment
                  Just (Tuple path id) -> unsafePerformEff $ insert l path (Tuple (id) (id+1)) tree
              ) emptyTree $ A.zip letters charsWithPrevious

        L.print result === str

    it "inserts arbitrary strings character by character from the end" do
      emptyTree <- liftEff $ newCharTree

      quickCheck' 666 \str -> do
        let chars = A.zip (A.reverse $ S.toCharArray str) (A.range 0 $ S.length str)
            letters = map (\(Tuple c id) -> makeLetter c id) chars
            charsWithPrevious = [Nothing] <> map Just letters
            result = scanl (
              \tree (Tuple l prev) -> case prev of
                Nothing -> unsafePerformEff $ L.insert l Nil (Tuple 0 capacity) tree
                Just l' -> case findPath l'.id tree of
                  Nothing -> trace "Didn't find a path" \_ -> tree
                  -- we need to define a lens to zoom in on this fragment
                  Just (Tuple path id) -> unsafePerformEff $ L.insert l path (Tuple (id-1) (id-1)) tree
              ) emptyTree $ A.zip letters charsWithPrevious
            lastR = fromMaybe emptyTree $ A.last result
        let a = unsafePerformEff $ whenM (pure $ L.print lastR /= str)
                                            (log $ foldl (\acc r -> acc <> L.draw 0 r <> "\n") "\n\n" result)
        L.print lastR === str
