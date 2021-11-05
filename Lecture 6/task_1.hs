{-# OPTIONS_GHC -Wall #-}

-- Naive mean implementation.
naiveMean :: Fractional a => [a] -> a
naiveMean xs = sum xs / fromIntegral (length xs)

-- | A mean with a number of values this mean was computed for.
data Mean a = Mean Int a
  deriving (Show)
  
-- Semigroup implementation for mean
instance Fractional a => Semigroup (Mean a) where
  Mean n x <> Mean 0 _ = Mean n x
  Mean 0 _ <> Mean n x = Mean n x
  Mean n x <> Mean m y = Mean (n + m) (x + (y - x) * (fromIntegral m) / fromIntegral(n + m))

-- Monoid implementation for mean
instance Fractional a => Monoid (Mean a) where
  mempty = Mean 0 0

-- mean of [array] of numbers implementation.
mean :: Fractional a => [a] -> Mean a
mean = mconcat . map (Mean 1)

 -- | Indermediate results for
-- count, mean and variance.
data Variance a = Variance Int a a
  deriving (Show)
  
-- implementation of monoid for Variance.
instance Fractional a => Monoid (Variance a) where
  mempty = Variance 0 0 0
 
-- implementation of semigroup for Variance.
instance Fractional a => Semigroup (Variance a) where
  Variance 0 _ _ <> Variance countB meanB varianceB = Variance countB meanB varianceB
  Variance countA meanA varianceA <> Variance 0 _ _ = Variance countA meanA varianceA
  Variance countA meanA varianceA <> Variance countB meanB varianceB = Variance (countA + countB) meanC varianceC
    where
      delta= meanB - meanA
      floatCountA = fromIntegral countA
      floatCountB = fromIntegral countB
      floatCountC = floatCountA + floatCountB
      meanC = meanA + (delta * floatCountB / floatCountC)
      varianceC = (varianceA * (floatCountA - 1) + varianceB * (floatCountB - 1) + delta * delta * floatCountA * floatCountB / floatCountC) /
          (floatCountA + floatCountB - 1)

-- | Compute count, mean and variance
-- for a list of values.
variance :: Fractional a => [a] -> Variance a
variance = mconcat . map (\x -> Variance 1 x 0)

main :: IO()
main = do
   putStrLn(show(variance [1, 3, 5]))


