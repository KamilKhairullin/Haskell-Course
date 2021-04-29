{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

data Expr a
  = Lit Int -- ˆ Integer literal.
  | Var a -- ˆ Variable
  | Add (Expr a) (Expr a) -- ˆ Addition.
  | Mul (Expr a) (Expr a) -- ˆ Multiplication.
  -- 1.3 added DIv and Mod
  | Div (Expr a) (Expr a) -- ˆ Div
  | Mod (Expr a) (Expr a) -- ˆ Mod
  deriving (Show, Functor)

eval :: Expr Int -> Int
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
eval (Mod e1 e2) = eval e1 `mod` eval e2


-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list).
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith _ _ (Lit a) = a
evalWith dflt list (Var a) = case (lookup a list) of
  Just value -> value
  Nothing -> dflt
evalWith dflt list (Add a b) = evalWith dflt list a + evalWith dflt list b
evalWith dflt list (Mul a b) = evalWith dflt list a * evalWith dflt list b
evalWith dflt list (Div a b) = evalWith dflt list a `div` evalWith dflt list b
evalWith dflt list (Mod a b) = evalWith dflt list a `mod` evalWith dflt list b


-- | Display an expression using a given
-- display function for variables.
displayWith :: (var -> String) -> Expr var -> String
displayWith f (Lit a) = show a
displayWith f (Var a) = f a
displayWith f (Add a b) = "(" ++ displayWith f a ++ "+" ++ displayWith f b ++ ")"
displayWith f (Mul a b) = "(" ++ displayWith f a ++ "*" ++ displayWith f b ++ ")"
displayWith f (Div a b) = "(" ++ displayWith f a ++ "div" ++ displayWith f b ++ ")"
displayWith f (Mod a b) = "(" ++ displayWith f a ++ "mod" ++ displayWith f b ++ ")"

-- |
expandVars :: Eq var => a -> [(var , a)] -> Expr var -> Expr a
expandVars dflt list expr = fmap (findInList dflt list) expr

findInList ::  Eq var => a -> [(var , a)] -> var -> a
findInList dflt list elementToFind = case (lookup elementToFind list) of
  Just value -> value
  Nothing -> dflt

-- 1.1 tests
evalWith_test_1 = evalWith 0 [("x", 2), ("y", 3)] (Add (Var "x") (Var "y"))
evalWith_test_2 = evalWith 0 [("x", 2), ("y", 3)] (Add (Mul ((Add (Var "x") (Var "y"))) ((Add (Var "x") (Var "y")))) (Var "z"))
displayWith_test_3 = displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))
displayWith_test_4 = displayWith show (Mul ((Add (Var "x") (Var "y"))) (Add (Lit 2) ((Mul (Var "y") (Var "y")))))

-- 1.2 tests
test_5 = displayWith show (expandVars (Var "<unknown>") [( ("x"), (Add (Var "y") (Var "z")) ), (("y"), (Add (Var "x") (Lit 3)) )] (Mul (Var "x") (Var "y")))
test_6 = displayWith show (expandVars (Var "<unknown>") [( ("x"), (Add (Var "y") (Var "z")) ), (("y"), (Add (Var "x") (Lit 3)) )] equation)
  where
    equation = (Mul ((Add (Var "y") (Var "z"))) ((Add (Var "x") (Lit 3))))

data GExpr f a
  = GVar a -- variable
  | GOp (f  a) -- generalised operation/literal

data IExpr expr
  = ILit Int
  | IVar expr
  | IAdd expr expr
  | IMul expr expr
  | IDiv expr expr
  | IMod expr expr
  deriving (Show, Functor)

-- | Convert from simple to generalised expression.
fromExpr :: Expr Int -> GExpr IExpr Int
fromExpr (Var a) = GVar a
fromExpr (Lit a) = GOp(ILit a)
fromExpr (Add a b) = GOp (IAdd (eval a) (eval b))
fromExpr (Mul a b) = GOp (IMul (eval a) (eval b))
fromExpr (Div a b) = GOp (IDiv (eval a) (eval b))
fromExpr (Mod a b) = GOp (IMod (eval a) (eval b))


-- | Convert from generalised to simple expression.
toExpr :: GExpr IExpr Int -> Expr Int
toExpr (GVar a)  = (Var a)
toExpr (GOp(ILit a)) = (Lit a)
toExpr (GOp (IAdd a b)) = (Add (Lit a) (Lit b))
toExpr (GOp (IMul a b))  = (Mul (Lit a) (Lit b))
toExpr (GOp (IDiv a b)) = (Div (Lit a) (Lit b))
toExpr (GOp (IMod a b) ) = (Mod (Lit a) (Lit b))


main :: IO ()
main = do
  putStrLn (show (IAdd 4 5))
