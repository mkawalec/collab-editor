module Test.Main where

import Prelude

import Control.Comonad.Env (mapEnvT)
import Control.Monad (whenM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Foldable (foldl, foldr, foldMap)
import Data.LSEQ as L
import Data.LSEQ.Helpers (newCharTree)
import Data.LSEQ.Types (Container, CharTree(..), Position(..), class CharTreeDisplay, displayElement)
import Data.LSEQ.Utility as LU
import Data.List (List(..), init, last, head)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Traversable (scanl, traverse, foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (trace)
import Test.QuickCheck ((===))
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

makeLetter :: Char -> Int -> Container Int OurChar
makeLetter l id = {id: Just id, payload: Just (OurChar l), subtree: Leaf}

newtype OurChar = OurChar Char

instance displayChar :: CharTreeDisplay OurChar where
  displayElement (OurChar c) = S.singleton c

instance showOurChar :: Show OurChar where
  show (OurChar c) = show c

derive instance eqOurChar :: Eq OurChar

pathToPos :: List Int -> Maybe (Tuple (List Int) Int)
pathToPos path = let pathToNode = fromMaybe Nil (init path) in
                 (Tuple pathToNode) <$> last path

main :: Eff (RunnerEffects (QCRunnerEffects (random :: RANDOM))) Unit
main = run [consoleReporter] do
  -- TODO: We should add an arbitrary instance of CharTree, but YOLO
  describe "LSEQ" do

    describe "folds" do
      let emptyTree = unsafePerformEff $ CharTree <$> newCharTree
          (Tuple tree pos) = unsafePerformEff $ L.insert (makeLetter 'b' 0) Nil (Tuple (N 1) End) emptyTree
          (Tuple tree' pos) = unsafePerformEff $ L.insert (makeLetter 'c' 1) Nil (Tuple (N 1) End) tree

      it "should foldl" do
        (foldl (\acc _ -> acc + 1) 0 tree') `shouldEqual` 2

      it "should foldr" do
        (foldr (\_ acc -> acc + 1) 0 tree') `shouldEqual` 2

      it "foldMaps" do
        (foldMap (const [2]) tree') `shouldEqual` [2,2]

    describe "print" do
      pending "prints an empty tree"
      it "prints an arbitrary tree" do
        emptyTree <- liftEff $ CharTree <$> newCharTree

        quickCheck \str -> do
          let chars = A.zip (A.reverse $ S.toCharArray str) (A.range 0 $ S.length str)
              letters = map (\(Tuple c id) -> makeLetter c id) chars
              result = scanl (
                \(Tuple tree prev) l -> case prev of
                  Nothing -> bimap id Just $ unsafePerformEff $ L.insert l Nil (Tuple (N 0) End) tree
                  Just path -> case pathToPos path of
                    Just (Tuple path' x) -> bimap id Just $ unsafePerformEff $ L.insert l path' (Tuple (N 0) (N $ x-1)) tree
                    Nothing -> trace "can't find path" \_ -> (Tuple tree Nothing)
                ) (Tuple emptyTree Nothing) $ letters
              lastR = fromMaybe emptyTree (fst <$> A.last result)
              {cache} = LU.print lastR

          -- tests should return a monad, no way to check multiple conditions
          true === (A.all id $ map (case _ of
              (Tuple (Tuple _ (Just path)) id) -> (M.lookup id cache) == (Just path)
              _ -> true
            ) (A.zip result $ A.range 0 (A.length result - 1)))
          --(fst $ printed) === str


    describe "delete" do
      it "should delete a single node" do
        emptyTree <- liftEff $ CharTree <$> newCharTree
        (Tuple tree pos) <- liftEff $ L.insert (makeLetter 'b' 0) Nil (Tuple (N 1) End) emptyTree

        let deleted = L.delete Nil (fromMaybe 0 $ head pos) tree

        case deleted of
          Leaf -> fail "deletion should leave a CharTree"
          CharTree tree -> M.isEmpty tree.items `shouldEqual` true

      it "perserves the original tree when there is nothing to delete" do
        emptyTree <- liftEff $ CharTree <$> newCharTree
        (Tuple tree pos) <- liftEff $ L.insert (makeLetter 'b' 0) Nil (Tuple (N 1) End) emptyTree
        (Tuple tree' pos) <- liftEff $ L.insert (makeLetter 'c' 1) Nil (Tuple (N 1) End) tree

        let deleted = L.delete Nil 0 tree'
        -- a bypass of the shitty Show instance situation
        LU.draw 0 deleted `shouldEqual` LU.draw 0 tree'

      it "should preserve a carrier parent node" do
        emptyTree <- liftEff $ CharTree <$> newCharTree
        (Tuple tree pos) <- liftEff $ L.insert (makeLetter 'b' 0) Nil (Tuple (N 1) End) emptyTree
        let pos' = fromMaybe 0 $ head pos
        (Tuple tree' sndPos) <- liftEff $ L.insert (makeLetter 'c' 1) Nil (Tuple (N pos') (N pos')) tree

        let deleted = L.delete Nil pos' tree'
        case deleted of
          Leaf -> fail "deletion should leave a CharTree"
          CharTree t -> case M.lookup pos' t.items of
            Nothing -> fail "the carrier parent should be preserved"
            Just n  -> do
              n.id `shouldEqual` Nothing
              n.payload `shouldEqual` Nothing

              let nodeId = fromMaybe 0 $ last sndPos
              case n.subtree of
                Leaf -> fail "the subtree shouldn't be a leaf"
                CharTree subtree -> case M.lookup nodeId subtree.items of
                  Nothing -> fail "Child node got lost somewhere"
                  Just node -> do
                    node.id `shouldEqual` Just 1
                    node.payload `shouldEqual` (Just $ OurChar 'c')

      pending "should delete with an empty child node"

    describe "insert" do
      it "inserts a single character" do

        emptyTree <- liftEff $ CharTree <$> newCharTree
        (Tuple withA pos) <- liftEff $ L.insert (makeLetter 'a' 0) Nil (Tuple (N 1) End) emptyTree

        case withA of
          Leaf          -> fail "The tree shouldn't be empty after insertion"
          CharTree tree -> case M.lookup (fromMaybe 0 $ head pos) tree.items of
            Nothing -> fail "letter wasn't found"
            Just l  -> do
              l.id `shouldEqual` Just 0
              l.payload `shouldEqual` (Just $ OurChar 'a')

        (_.string $ LU.print withA) `shouldEqual` "a"

      it "inserts arbitrary strings character by character" do
        emptyTree <- liftEff $ CharTree <$> newCharTree

        quickCheck \str -> do
          let chars = A.zip (S.toCharArray str) (A.range 0 $ S.length str)
              letters = map (\(Tuple c id) -> makeLetter c id) chars
              result = map fst $ scanl (
                \(Tuple tree prev) l -> case prev of
                  Nothing -> bimap id Just $ unsafePerformEff $ L.insert l Nil (Tuple (N 0) End) tree
                  Just path -> case pathToPos path of
                    Just (Tuple path' x) -> bimap id Just $ unsafePerformEff $ L.insert l path' (Tuple (N x) End) tree
                    Nothing -> trace "can't find path" \_ -> (Tuple tree Nothing)
                ) (Tuple emptyTree Nothing) $ letters
              lastR = fromMaybe emptyTree $ A.last result

          -- I love how awful this is :D
          let a = unsafePerformEff $ whenM (pure $ (_.string $ LU.print lastR) /= str)
                                              (log $ foldl (\acc r -> acc <> LU.draw 0 r <> "\n") "\n\n" result)
          (_.string $ LU.print lastR) === str

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
          let a = unsafePerformEff $ whenM (pure $ (_.string $ LU.print lastR) /= str)
                                              (log $ foldl (\acc r -> acc <> LU.draw 0 r <> "\n") "\n\n" result)
          (_.string $ LU.print lastR) === str
