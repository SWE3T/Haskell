module Lexer where

import Data.Char

data Expr  = Var String
           | Num Int
           | Lam String Ty Expr
           | BTrue 
           | BFalse
           | Or Expr Expr  
           | And Expr Expr 
           | Add Expr Expr
           | Times Expr Expr
           | App Expr Expr 
           | Let String Expr Expr
           | Record [(String, Expr)] 
           | AcessRecord Expr String 
           deriving Show

-- data Record = String Expr
--             deriving Show


data Ty    = TBool
           | TNum
           | TVar
           | TFun Ty Ty
           deriving (Show, Eq)

data Token = TokenNum Int 
           | TokenVar String
           | TokenBTrue 
           | TokenBFalse 
           | TokenAnd
           | TokenOr
           | TokenPlus
           | TokenTimes
           | TokenLBracket
           | TokenAssign
           | TokenComma
           | TokenRBracket
           | TokenLet
           | TokenIn
           | TokenDot
           deriving Show 

-- Função que recebe o código e retorna uma lista de Tokens 
lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus       : lexer cs
lexer ('*':cs) = TokenTimes      : lexer cs
lexer ('|':cs) = TokenOr         : lexer cs
lexer ('&':cs) = TokenAnd        : lexer cs
lexer ('=':cs) = TokenAssign     : lexer cs
lexer (',':cs) = TokenComma      : lexer cs
lexer ('{':cs) = TokenLBracket   : lexer cs
lexer ('}':cs) = TokenRBracket   : lexer cs
lexer ('.':cs) = TokenDot        : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexReserved (c:cs)
lexer _ = error "Erro Léxico: caractere inválido"

lexReserved cs = case span isAlpha cs of
                   ("let", rest)    -> TokenLet     : lexer rest
                   ("in", rest)     -> TokenIn      : lexer rest
                   ("true", rest)   -> TokenBTrue   : lexer rest
                   ("false", rest)  -> TokenBFalse  : lexer rest
                   (var, rest)      -> TokenVar var : lexer rest
                   --_ -> error "Erro Léxico: nenhum correspondente para isso."             

lexNum cs = case span isDigit cs of 
                   (num, rest) -> TokenNum (read num) : lexer rest


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





-- typeof :: Expr -> Maybe Ty 
-- typeof BTrue = Just TBool 
-- typeof BFalse = Just TBool
-- typeof (Num _) = Just TNum
-- typeof (Add e1 e2) = case (typeof e1) of 
--                 Just TNum -> case (typeof e2) of 
--                     Just TNum -> Just TNum
--                     _         -> Nothing
--                 _ -> Nothing
-- typeof (And e1 e2) = case (typeof e1, typeof e2) of 
--                         (Just TBool, Just TBool) -> Just TBool
--                         _                        -> Nothing 





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