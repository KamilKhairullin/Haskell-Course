{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

data Expr a
  = Lit Int -- ˆ Integer literal.
  | Var a -- ˆ Variable
  | Add (Expr a) (Expr a) -- ˆ Addition.
  | Mul (Expr a) (Expr a) -- ˆ Multiplication.
  deriving (Show, Functor)

-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list).
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith dflt list (Lit a) = a
evalWith dflt list (Var a) = case (lookup a list) of
  Just value -> value
  Nothing -> dflt
evalWith dflt list (Add a b) = evalWith dflt list a + evalWith dflt list b
evalWith dflt list (Mul a b) = evalWith dflt list a * evalWith dflt list b

-- | Display an expression using a given
-- display function for variables.
displayWith :: (var -> String) -> Expr var -> String
displayWith f (Lit a) = show a
displayWith f (Var a) = f a
displayWith f (Add a b) = "(" ++ displayWith f a ++ "+" ++ displayWith f b ++ ")"
displayWith f (Mul a b) = "(" ++ displayWith f a ++ "*" ++ displayWith f b ++ ")"


evalWith_test_1 = evalWith 0 [("x", 2), ("y", 3)] (Add (Var "x") (Var "y"))
evalWith_test_2 = evalWith 0 vars ((x + y) *(x + y) + z)
evalWith_test_3 = displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))
evalWith_test_4 = displayWith display (Mul (Var (x + y)) (2 + Var (yˆ2)))


main :: IO ()
main = do
  putStrLn (show(evalWith_test_1))
{-
  putStrLn (show(evalWith_test_2))
  putStrLn (show(evalWith_test_3))
  putStrLn (show(evalWith_test_4))
-}
