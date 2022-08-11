module TypeChecker where

import Data.List

import Lexer

type Ctx = [(String, Ty)]


typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Var v) = lookup v ctx
typeof ctx (Num _) = Just TNum
typeof ctx (Record _) = Just TRecs

typeof ctx (Lam v t1 b) = let ctx' = (v, t1):ctx
                              Just t2 = typeof ctx' b
                            in Just (TFun t1 t2)
typeof ctx (App e1 e2) = case (typeof ctx e1, typeof ctx e2) of
          (Just (TFun t11 t12), Just t2) -> if (t11 == t2) then
                Just t12
                  else
                Nothing  
          _ -> Nothing

typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum) -> Just TNum
            _                      -> Nothing
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TNum, Just TNum) -> Just TNum
            _                      -> Nothing
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TBool, Just TBool) -> Just TBool
            _                        -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
            (Just TBool, Just TBool) -> Just TBool
            _                        -> Nothing
-- typeof ctx (Let e1 e2) = case (typeof  ctx e1, typeof ctx e2) of 
--             (Just TBool, Just TBool) -> Just TBool
--             _                        -> Nothing
-- typeof ctx (AcessRecord e1 e2) = (typeof ctx (lookup e2))
-- typeof ctx (AcessRecord e1 e2) = case (typeof  ctx e1, typeof ctx e2) of 
--             (Just TRecs, Just TBool) -> Just TBool
--             _                        -> Nothing



typeof ctx (AcessRecord e1 e2) = case (typeof ctx e1, e2) of  
              (Just TRecs, String) -> let e = lookup e2 e1 
                                in case e of  
                                  Just x  -> typeof ctx x 
                                  Nothing -> Nothing
              _                -> Nothing                      
          -- (Just TRecs, Just t2) -> Just t2
          -- _ -> Nothing



typecheck :: Expr -> Expr
typecheck exp = case (typeof [] exp) of 
    Just _     -> exp
    _          -> error "ERROR! Tipos distintos na operação!" 


    -- step :: Expr -> Maybe Expr


-- step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
-- step (Add (Num n1) e2) = case (step e2) of 
--                         Just e2' -> Just (Add (Num n1) e2')
--                         Nothing  -> Nothing
-- step (Add e1 e2)       = case (step e1) of
--                         Just e1' -> Just (Add e1' e2)
--                         Nothing  -> Nothing
-- step (And BTrue e2)    = Just e2
-- step (And BFalse _)    = Just BFalse
-- step (And e1 e2)       = case (step e1) of
--                        Just e1' -> Just (And e1' e2)
--                        Nothing  -> Nothing
 

-- eval :: Expr -> Expr 
-- eval e | is_value e = e
--        | otherwise  = eval (step e)


-- is_value :: Expr -> Bool
-- is_value BFalse = False
-- is_value BTrue = True
-- is_value (Lam _ _ _) = True
-- is_value _ = False