module Interpreter where

import Lexer

import Data.Char

-- Substituição da variável
subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then
                        n
                      else 
                        b
subst x n (Lam v t b)         = Lam v t (subst x n b)
subst x n (App e1 e2)         = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2)         = Add (subst x n e1) (subst x n e2)
subst x n (Minus e1 e2)       = Minus (subst x n e1) (subst x n e2)
subst x n (Times e1 e2)       = Times (subst x n e1) (subst x n e2)
subst x n (Greater e1 e2)     = Greater (subst x n e1) (subst x n e2)
subst x n (Equal e1 e2)       = Equal (subst x n e1) (subst x n e2)
subst x n (Different e1 e2)   = Different (subst x n e1) (subst x n e2)
subst x n (And e1 e2)         = And (subst x n e1) (subst x n e2)
subst x n (Or e1 e2)          = Or (subst x n e1) (subst x n e2)
subst x n (Let v e1 e2)       = Let v (subst x n e1) (subst x n e2)
subst x n (AcessRecord e1 e2) = AcessRecord (subst x n e1) e2
subst x n (Ternary c e1 e2)   = Ternary c (subst x n e1) (subst x n e2)
subst x n (Paren e1)          = Paren (subst x n e1)
subst x n e = e

-- Avaliação da expressão
is_value :: Expr -> Bool
is_value (Lam _ _ _) = True
is_value (Record _)  = True
is_value (Var _)     = True
is_value (Num _)     = True
is_value BFalse      = True
is_value BTrue       = True
is_value _           = False


-- Processamento da expressão
step :: Expr -> Expr
step (App e1@(Lam x _ b) e2) | is_value e2 = subst x e2 b 
                             | otherwise   = (App e1 (step e2))
step (App e1 e2)                   = App (step e1) e2 

step (Add (Num n1) (Num n2))       = Num (n1 + n2)
step (Add (Num n1) e2)             = Add (Num n1) (step e2)
step (Add e1 e2)                   = Add (step e1) e2

step (Minus (Num n1) (Num n2))     = Num (n1 - n2)
step (Minus (Num n1) e2)           = Minus (Num n1) (step e2)
step (Minus e1 e2)                 = Minus (step e1) e2

step (Times (Num n1) (Num n2))     = Num (n1 * n2)
step (Times (Num n1) e2)           = Times (Num n1) (step e2)
step (Times e1 e2)                 = Times (step e1) e2

step (Greater (Num e1) (Num e2))   = if (e1 >  e2) then BTrue else BFalse
step (Greater (Num e1) e2)         = Greater (Num e1) (step e2)
step (Greater e1 e2)               = Greater (step e1) e2

step (Smaller (Num e1) (Num e2))   = if (e1 < e2) then BTrue else BFalse
step (Smaller (Num e1) e2)         = Smaller (Num e1) (step e2)
step (Smaller e1 e2)               = Smaller (step e1) e2

step (Equal (Num e1) (Num e2))     = if (e1 == e2) then BTrue else BFalse
step (Equal (Num e1) e2)           = Equal (Num e1) (step e2)
step (Equal e1 e2)                 = Equal (step e1) e2

step (Different (Num e1) (Num e2)) = if (e1 /= e2) then BTrue else BFalse
step (Different (Num e1) e2)       = Different (Num e1) (step e2)
step (Different e1 e2)             = Different (step e1) e2

step (And BTrue BTrue)             = BTrue
step (And BTrue BFalse)            = BFalse
step (And BFalse _ )               = BFalse
step (And BTrue e2 )               = And BTrue (step e2)
step (And e1 e2)                   = And (step e1) e2

step (Or BFalse BFalse)            = BFalse
step (Or BFalse BTrue )            = BTrue
step (Or BTrue _ )                 = BTrue
step (Or BFalse e2)                = Or BFalse (step e2)
step (Or e1 e2 )                   = Or (step e1) e2

step (Paren e1)                    = (step e1)

step (Let v e1 e2) | is_value e1   = subst v e1 e2
                   | is_record e1  = Let v (step (AcessRecord e1 v)) e2 
                   | otherwise     = Let v (step e1) e2

-- step (Ternary c@(AcessRecord l v) e1 e2) = Ternary (step (AcessRecord l v)) e1 e2 
step (Ternary c e1 e2) = let s = eval c
                        in case s of
                          BTrue  -> e1 
                          BFalse -> e2
                          otherwise -> error "ERROR: Condicional incompreensível."

step (AcessRecord (Record list) v) = let val = lookup v list 
                                  in case val of  
                                    Just x  -> x 
                                    Nothing -> error "ERROR: Não existe este elemento no Record" 
step e = e 


is_record (Record _)  = True
is_record _           = False


-- Chamada recursiva até obter o valor final
eval :: Expr -> Expr 
eval e | is_value e = e
       | otherwise  = eval (step e)