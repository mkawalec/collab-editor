module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as A
import Data.Foldable (foldl)
import Data.List (List(..), init, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Bifunctor (bimap)
import Data.String as S
import Data.Traversable (scanl)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit(..))
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Control.Monad (whenM)
import Debug.Trace (trace)

import LSEQ as L
import LSEQ.Utility as LU
import LSEQ.Helpers (newCharTree)
import LSEQ.Types (Container, CharTree(..), Position(..), class CharTreeDisplay, displayElement)

makeLetter :: Char -> Int -> Container Int OurChar
makeLetter l id = {id: Just id, payload: Just (OurChar l), subtree: Leaf}

newtype OurChar = OurChar Char

instance displayChar :: CharTreeDisplay OurChar where
  displayElement (OurChar c) = S.singleton c

pathToPos :: List Int -> Maybe (Tuple (List Int) Int)
pathToPos path = let pathToNode = fromMaybe Nil (init path) in
                 (Tuple pathToNode) <$> last path

main :: Eff (RunnerEffects (QCRunnerEffects (random :: RANDOM))) Unit
main = run [consoleReporter] do
  describe "LSEQ" do
    it "inserts a single character" do

      emptyTree <- liftEff $ CharTree <$> newCharTree
      (Tuple withA pos) <- liftEff $ L.insert (makeLetter 'a' 0) Nil (Tuple (N 1) (N 1)) emptyTree
      LU.print withA `shouldEqual` "a"

    it "inserts arbitrary strings character by character" do
      emptyTree <- liftEff $ CharTree <$> newCharTree

      quickCheck \str -> do
        let chars = A.zip (S.toCharArray str) (A.range 0 $ S.length str)
            letters = map (\(Tuple c id) -> makeLetter c id) chars
            result = map fst $ scanl (
              \(Tuple tree prev) l -> case prev of
                Nothing -> bimap id Just $ unsafePerformEff $ L.insert l Nil (Tuple (N 0) End) tree
                Just path -> case pathToPos path of
                  Just (Tuple path' x) -> bimap id Just $ unsafePerformEff $ L.insert l path' (Tuple (N x) (N $ x+1)) tree
                  Nothing -> trace "can't find path" \_ -> (Tuple tree Nothing)
              ) (Tuple emptyTree Nothing) $ letters
            lastR = fromMaybe emptyTree $ A.last result

        let a = unsafePerformEff $ whenM (pure $ LU.print lastR /= str)
                                            (log $ foldl (\acc r -> acc <> LU.draw 0 r <> "\n") "\n\n" result)
        LU.print lastR === str

    it "inserts arbitrary strings character by character from the end" do
      emptyTree <- liftEff $ CharTree <$> newCharTree

      quickCheck \str -> do
        let chars = A.zip (A.reverse $ S.toCharArray str) (A.range 0 $ S.length str)
            letters = map (\(Tuple c id) -> makeLetter c id) chars
            result = map fst $ scanl (
              \(Tuple tree prev) l -> case prev of
                Nothing -> bimap id Just $ unsafePerformEff $ L.insert l Nil (Tuple (N 0) End) tree
                Just path -> case pathToPos path of
                  Just (Tuple path' x) -> bimap id Just $ unsafePerformEff $ L.insert l path' (Tuple (N 0) (N $ x-1)) tree
                  Nothing -> trace "can't find path" \_ -> (Tuple tree Nothing)
              ) (Tuple emptyTree Nothing) $ letters
            lastR = fromMaybe emptyTree $ A.last result
        let a = unsafePerformEff $ whenM (pure $ LU.print lastR /= str)
                                            (log $ foldl (\acc r -> acc <> LU.draw 0 r <> "\n") "\n\n" result)
        LU.print lastR === str
