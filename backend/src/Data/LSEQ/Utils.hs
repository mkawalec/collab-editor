module Data.LSEQ.Utils (
  kumaraswamy
) where

import System.Random (RandomGen, random)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

-- | Get a number from a Kumaraswamy distribution
kumaraswamy :: RandomGen g => g -> Double -> Double -> (Double, g)
kumaraswamy gen a b = mapFst (\n -> (1 - (1 - n) ** (1 / b)) ** 1 / a) $ random gen
