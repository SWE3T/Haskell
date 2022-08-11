module TypeChecker where

import Data.List

import Lexer

-- Definindo o Contexto
type Ctx = [(String, Ty)]

-- Análise do Tipo do elemento
typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx BTrue      = Just TBool
typeof ctx BFalse     = Just TBool
typeof ctx (Var v)    = lookup v ctx
typeof ctx (Num _)    = Just TNum
typeof ctx (Record _) = Just TRecs

typeof ctx (Lam v t1 b) = let ctx' = (v, t1):ctx
                              Just t2 = typeof ctx' b
                            in Just (TFun t1 t2)
typeof ctx (App e1 e2)  = case (typeof ctx e1, typeof ctx e2) of
          (Just (TFun t11 t12), Just t2) -> if (t11 == t2) then
                Just t12
                  else
                Nothing  
          _ -> Nothing

typeof ctx (Add e1 e2)   = case (typeof ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum)   -> Just TNum
            _                        -> Nothing
typeof ctx (Minus e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum)   -> Just TNum
            _                        -> Nothing
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum)   -> Just TNum
            _                        -> Nothing
typeof ctx (Or e1 e2)    = case (typeof ctx e1, typeof ctx e2) of 
            (Just TBool, Just TBool) -> Just TBool
            _                        -> Nothing
typeof ctx (And e1 e2)   = case (typeof ctx e1, typeof ctx e2) of 
            (Just TBool, Just TBool) -> Just TBool
            _                        -> Nothing



typeof ctx (Let x e1 e2) = let Just e    = typeof ctx e1
            in typeof ((x, e):ctx) e2

typeof ctx (AcessRecord (Record list) v) = let val = lookup v list 
            in case val of  
              Just x  -> typeof ctx x 
              _       -> Nothing 
typeof ctx (AcessRecord (Var x) v)       = lookup x ctx 
typeof ctx (AcessRecord _ _)             = Nothing 


typecheck :: Expr -> Expr
typecheck exp = case (typeof [] exp) of 
    Just _     -> exp
    _          -> error "ERROR! Tipos distintos na operação!" 
