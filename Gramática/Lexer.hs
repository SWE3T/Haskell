module Lexer where

import Data.Char

--Tipos da gramática
data Ty    = TBool
           | TNum
           | TVar
           | TRecs 
           | TFun Ty Ty
           deriving (Show, Eq)

--Expressões da gramática
data Expr  = Var String
           | Num Int
           | BTrue 
           | BFalse
           | Or Expr Expr  
           | And Expr Expr 
           | Add Expr Expr
           | Minus Expr Expr
           | Times Expr Expr
           | Equal Expr Expr
           | Different Expr Expr
           | Greater Expr Expr
           | Smaller Expr Expr
           | App Expr Expr 
           | Lam String Ty Expr
           | Let String Expr Expr
           | Record [(String, Expr)] 
           | AcessRecord Expr String 
           | Ternary Expr Expr Expr 
           | Paren Expr  
           deriving Show

--Tokens da Gramática
data Token = TokenNum Int 
           | TokenVar String
           | TokenBTrue 
           | TokenBFalse 
           | TokenAnd
           | TokenOr
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenEqual
           | TokenDiff
           | TokenGreater
           | TokenSmaller
           | TokenLBracket
           | TokenAssign
           | TokenComma
           | TokenRBracket
           | TokenLParen
           | TokenRParen 
           | TokenLet
           | TokenIn
           | TokenDot
           | TokenDDots
           | TokenQMark
           deriving Show 

-- Função que recebe o código e retorna uma lista de Tokens 
lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus       : lexer cs
lexer ('-':cs) = TokenMinus      : lexer cs
lexer ('*':cs) = TokenTimes      : lexer cs
lexer ('>':cs) = TokenGreater    : lexer cs
lexer ('<':cs) = TokenSmaller    : lexer cs
lexer ('|':cs) = TokenOr         : lexer cs
lexer ('&':cs) = TokenAnd        : lexer cs
lexer ('=':cs) = TokenAssign     : lexer cs
lexer (',':cs) = TokenComma      : lexer cs
lexer ('{':cs) = TokenLBracket   : lexer cs
lexer ('}':cs) = TokenRBracket   : lexer cs
lexer ('(':cs) = TokenLParen     : lexer cs
lexer (')':cs) = TokenRParen     : lexer cs
lexer ('.':cs) = TokenDot        : lexer cs
lexer (':':cs) = TokenDDots      : lexer cs
lexer ('?':cs) = TokenQMark      : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexReserved (c:cs)
lexer _ = error "Erro Léxico: caractere inválido"

lexReserved cs = case span isAlpha cs of
                   ("let", rest)        -> TokenLet      : lexer rest
                   ("in", rest)         -> TokenIn       : lexer rest
                   ("true", rest)       -> TokenBTrue    : lexer rest
                   ("false", rest)      -> TokenBFalse   : lexer rest
                   ("equal", rest)      -> TokenEqual    : lexer rest
                   ("different", rest)  -> TokenDiff     : lexer rest
                   (var, rest)          -> TokenVar var  : lexer rest
                   --_ -> error "Erro Léxico: nenhum correspondente para isso."             

lexNum cs = case span isDigit cs of 
                   (num, rest) -> TokenNum (read num) : lexer rest