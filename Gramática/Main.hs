module Main where

import Lexer

import Parser

--import Interpreter

--import TypeChecker


main = getContents >>= print . parser . lexer
--main = print "Foi"
-- lexer -> Parser -> Type -> Eval