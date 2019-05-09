module Chapter11.HuttonsRazor where

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n)     = n
eval (Add el er) = eval el + eval er

printExpr :: Expr -> String
printExpr (Lit n)     = show n
printExpr (Add el er) = printExpr el ++ " + " ++ printExpr er
