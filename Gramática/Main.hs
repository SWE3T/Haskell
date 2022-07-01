module Main where
import Lexer
import Parser

main = getContents >>= print . parser . lexer