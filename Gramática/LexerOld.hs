module Lexer where

import Data.Char

--Lista os tokens Válidos 
data Token = TokenNum Int 
           | TokenPlus
           | TokenTimes
           | TokenLParen
           | TokenRParen
           | TokenIf
           | TokenThen
           | TokenElse
           deriving Show 


data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          deriving Show



--Função que recebe o código e retorna uma lista de Tokens 
lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexReserved (c:cs)
lexer _ = error "Erro Léxico: caractere inválido"

lexReserved cs = case span isAlpha cs of
                   ("if", rest) -> TokenIf : lexer rest
                   ("then", rest) -> TokenThen : lexer rest
                   ("else", rest) -> TokenElse : lexer rest
                   _ -> error "Erro Léxico: nenhum correspondente para isso."             

lexNum cs = case span isDigit cs of 
                (num, rest) -> TokenNum (read num) : lexer rest


step :: Expr -> Maybe Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) = case (step e2) of 
                        Just e2' -> Just (Add (Num n1) e2')
                        Nothing  -> Nothing
step (Add e1 e2)       = case (step e1) of
                        Just el' -> Just (Add e1' e2)
                        Nothing  -> Nothing
step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse
step (And e1 e2)    = case (step e1) of
                    Just e1' -> Just (And e1' e2)
                    Nothing  -> Nothing


typeof :: Expr -> Maybe Ty 
typeof BTrue = Just TBool 
typeof BFalse = Just TBool
typeof (Num _) = Just TNum
typeof (Add e1 e2) = case (typeof e1) of 
                Just TNum -> case (typeof e2) of 
                    Just TNum -> Just TNum
                    _         -> Nothing
                          -> Nothing
typeof (And e1 e2) = case (typeofe1, typeof e2) of 
                        (Just TBool, Just TBool) -> Just TBool
                        _                        -> Nothing 