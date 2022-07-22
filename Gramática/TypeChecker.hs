module TypeChecker where

import Data.List

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam v t1 b) = let ctx' = (v, t1):ctx
                              Just t2 = typeof ctx' b
                            in Just (TFun t1 t2)
typeof ctx (App e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
          (Just (TFun t11 t12), Just t2) -> if (t11 == t2) then
                Just t12
                  else
                Nothing  
          _ -> Nothing
typeof ctx (Add e1 e2) = case (typeof  ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum) -> Just TNum
            _                      -> Nothing


typecheck :: Expr -> Expr
typecheck exp = case (typeof [] exp) of 
    Just _     -> exp
    _          -> error "Erro na verificação de tipos!" 