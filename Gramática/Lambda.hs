module Lambda where

data Expr = Var String
          | Lam String Expr
          | App Expr Expr 
          deriving Show

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then
                        n
                      else 
                        b
subst x n (Lam v b) = Lam v (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)

is_value :: Expr -> Bool
is_value (Lam _ _) = True
is_value _ = False

eval :: Expr -> Expr
eval (App e1@(Lam x b) e2) | is_value e2 = subst x e2 b 
                           | otherwise   = (App e1 (eval e2))
eval (App e1 e2) = App (eval e1) e2 
eval e = e 

eval' :: Expr -> Expr 
eval' e | is_value e = e
        | otherwise  = eval' (eval e)