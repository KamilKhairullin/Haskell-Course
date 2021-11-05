# Task 1. Simple expressions
Implementation of data type representing simple expressions with variables

```
-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list).
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int

-- | Display an expression using a given
-- display function for variables.
displayWith :: (var -> String) -> Expr var -> String
```

Examples:
```
let vars = [("x", 2), ("y", 3)]
>>> evalWith 0 vars (Add (Var "x") (Var "y")) 
5
>>> evalWith 0 vars ((x + y)ˆ2 + z)
25
>>> displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))
"(\"x\") * (2 + \"y\")"
>>> displayWith display (Mul (Var (x + y)) (2 + Var (yˆ2)))
"(x + y) * (2 + (y) * (y))"
```