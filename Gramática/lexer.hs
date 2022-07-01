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