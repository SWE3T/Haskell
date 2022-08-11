module Main where

import Lexer

import Parser

import TypeChecker

import Interpreter


main = getContents >>= print . eval . parser . lexer
--main = print "Foi"
-- lexer -> Parser -> Type -> Eval