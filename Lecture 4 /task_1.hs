{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe

-- | Implies iteration of Newton's method
computeNextIteration :: Fractional a => (a -> a) -> (a -> a) -> a -> a
computeNextIteration f f' x = x - ((f x) / (f' x))

-- | Find a root of an equation
--
-- f(x) = 0
--
-- using Newton's method.
root ::
  Fractional a => Int
  -> (a -> Bool)
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Maybe (Int, a)

-- | Implementaion of finding root of equation
--
-- Using Newton method.
-- allAnswers - computed root for next maximumIterations
-- allNumeratedAnswers - pair of (Int, a). Numeration
-- persiceAnswers - answers, in which error is less than permissible
root maximumIterations isPercise f f'  initialRoot = listToMaybe persiceAnswers
  where
    allAnswers = take maximumIterations(iterate (computeNextIteration f f') initialRoot)
    allNumeratedAnswers = zip [0..] allAnswers
    persiceAnswers = dropWhile (\x -> err (snd x)) allNumeratedAnswers
    err x
      |isPercise (abs((computeNextIteration  f f' x)  - x)) = False
      | otherwise = True

main :: IO()
main = do
-- finding root for example (1)
    print(root 100 (< 1e-7) (\x -> x^2 - 2) (\x -> 2*x) 123)
-- finding root for example (2)
    print(root 100 (< 1e-12) cos (negate . sin) 1.0)
-- an example of a bad guess for example (2)
    print(root 100 (< 1e-12) cos (negate . sin) 0)