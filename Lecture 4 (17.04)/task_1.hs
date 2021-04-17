{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe

computeNextIteration :: Fractional a => (a -> a) -> (a -> a) -> a -> a
computeNextIteration f f' x = x - ((f x) / (f' x))

root ::
  Fractional a => Int
  -> (a -> Bool)
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Maybe (Int, a)

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
    print(root 100 (< 1e-7) (\x -> x^2 - 2) (\x -> 2*x) 123)
