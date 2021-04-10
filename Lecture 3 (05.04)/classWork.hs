{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
--import CodeWorld


initialState1 = 0

transitions1 0 = [('a', 1), ('b', 2)]
transitions1 1 = [('a', 2), ('b', 2)]
transitions1 _ = []

isFinal1 n = n >= 2

language
  :: state
  -> (state -> [(action, state)])
  -> (state -> Bool)
  ->  [[action]]

language initialState transitions isFinal
  | isFinal initialState = [[]]
  | otherwise = flatten
      (map (\(a, s) ->  (map (a:) (recurse s)))
          (transitions initialState))
    where
      recurse newState = language newState transitions isFinal

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

main :: IO()
main = do
    print("Language for FSM #1:")
    print (language initialState1 transitions1 isFinal1)
    
