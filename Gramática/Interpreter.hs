module Interpreter where

import Lexer

import Data.Char

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then
                        n
                      else 
                        b
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n (AcessRecord e1 e2) = AcessRecord (subst x n e1) e2
subst x n e = e


is_record :: Expr -> Bool
is_record (Record _) = True
is_record _          = False

is_value :: Expr -> Bool
is_value (Lam _ _ _) = True
is_value (Record _) = True
is_value (Var _) = True
is_value (Num _) = True
is_value BFalse = True
is_value BTrue = True
is_value _ = False


step :: Expr -> Expr
step (App e1@(Lam x _ b) e2) | is_value e2 = subst x e2 b 
                             | otherwise   = (App e1 (step e2))
step (App e1 e2) = App (step e1) e2 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2

step (And BFalse _ ) = BFalse
step (And BTrue e2 ) = e2
step (And e1 e2 ) = And (step e1) e2

step (Or BTrue _ ) = BTrue
step (Or BFalse e2 ) = e2
step (Or e1 e2 ) = Or (step e1) e2

step (Let v e1 e2) | is_record e1 = subst v e1 e2 
                   | is_value e1  = subst v e1 e2
                   | otherwise    = Let v (step e1) e2

step (AcessRecord (Record list) v) = let val = lookup v list 
                                   in case val of  
                                      Just x  -> x 
                                      Nothing -> error "ERRO: Não existe este elemento nos Records" 
step e = e 

eval :: Expr -> Expr 
eval e | is_value e = e
       | otherwise  = eval (step e)