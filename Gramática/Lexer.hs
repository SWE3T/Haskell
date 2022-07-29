module Lexer where

import Data.Char

data Expr = Var String
          | Lam String Ty Expr
          | App Expr Expr 
          | And Expr Expr 
          | Let String Expr Expr
          | Or Expr Expr  
          | BTrue 
          | BFalse
          | Num Int
          | Add Expr Expr
          | Times Expr Expr
          | Record String Expr 
          deriving Show

data Ty   = TBool
          | TNum
          | TVar
          | TFun Ty Ty
          deriving (Show, Eq)


data Token = TokenNum Int 
           | TokenVar String
           | TokenPlus
           | TokenTimes
           | TokenAnd
           | TokenOr
           | TokenLBracket
           | TokenAssign
           | TokenRBracket
           | TokenLet
           | TokenIn
           deriving Show 


-- Função que recebe o código e retorna uma lista de Tokens 
lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus  : lexer cs
lexer ('=':cs) = TokenAssign  : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('|':cs) = TokenOr    : lexer cs
lexer ('&':cs) = TokenAnd   : lexer cs
lexer ('{':cs) = TokenLBracket   : lexer cs
lexer ('}':cs) = TokenRBracket   : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexReserved (c:cs)
lexer _ = error "Erro Léxico: caractere inválido"

lexReserved cs = case span isAlpha cs of
                   ("let", rest) -> TokenLet : lexer rest
                   ("in", rest)  -> TokenIn  : lexer rest
                   (var,rest)    -> TokenVar var : lexer rest
                   _ -> error "Erro Léxico: nenhum correspondente para isso."             

lexNum cs = case span isDigit cs of 
                (num, rest) -> TokenNum (read num) : lexer rest


step :: Expr -> Maybe Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) = case (step e2) of 
                        Just e2' -> Just (Add (Num n1) e2')
                        Nothing  -> Nothing
step (Add e1 e2)       = case (step e1) of
                        Just e1' -> Just (Add e1' e2)
                        Nothing  -> Nothing
step (And BTrue e2)    = Just e2
step (And BFalse _)    = Just BFalse
step (And e1 e2)       = case (step e1) of
                       Just e1' -> Just (And e1' e2)
                       Nothing  -> Nothing


-- typeof :: Expr -> Maybe Ty 
-- typeof BTrue = Just TBool 
-- typeof BFalse = Just TBool
-- typeof (Num _) = Just TNum
-- typeof (Add e1 e2) = case (typeof e1) of 
--                 Just TNum -> case (typeof e2) of 
--                     Just TNum -> Just TNum
--                     _         -> Nothing
--                           -> Nothing
-- typeof (And e1 e2) = case (typeofe1, typeof e2) of 
--                         (Just TBool, Just TBool) -> Just TBool
--                         _                        -> Nothing 