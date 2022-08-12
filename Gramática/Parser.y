{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parserError }

%token 
    num        { TokenNum $$ }
    var        { TokenVar $$ }
    true       { TokenBTrue }
    false      { TokenBFalse }
    equal      { TokenEqual }
    different  { TokenDiff }
    '+'        { TokenPlus }
    '-'        { TokenMinus }
    '*'        { TokenTimes }
    '&'        { TokenAnd }
    '|'        { TokenOr }
    '>'        { TokenGreater }
    '<'        { TokenSmaller }
    '{'        { TokenLBracket }
    '='        { TokenAssign } 
    ','        { TokenComma } 
    '}'        { TokenRBracket }
    '('        { TokenLParen }
    ')'        { TokenRParen }
    '?'        { TokenQMark }
    ':'        { TokenDDots }
    '.'        { TokenDot }
    let        { TokenLet }
    in         { TokenIn }


%right in
%left '+'
%left '-'
%left '*' 
%left '&' 
%left '|' 
%left AcessRecord

%%


Exp : num                       { Num $1 }
    | var                       { Var $1 }
    | true                      { BTrue }
    | false                     { BFalse }
    | Exp '+' Exp               { Add $1 $3 }
    | Exp '-' Exp               { Minus $1 $3 }
    | Exp '*' Exp               { Times $1 $3 }
    | Exp '&' Exp               { And $1 $3 }
    | Exp '|' Exp               { Or $1 $3 }
    | Exp '>' Exp               { Greater $1 $3 }
    | Exp '<' Exp               { Smaller $1 $3 }
    | Exp equal Exp             { Equal $1 $3 }
    | Exp different Exp         { Different $1 $3 }
    | let var '=' Exp in Exp    { Let $2 $4 $6}
    | '{' RecordList '}'        { Record $2 }
    | '(' Exp ')'               { Paren $2 }
    | Exp'.'var                 { AcessRecord $1 $3 }
    | '('Exp')' '?' Exp ':' Exp { Ternary $2 $5 $7 }

RecordList : var '=' Exp                 { [($1, $3)] }
           | var '=' Exp ',' RecordList  { (($1, $3) : $5) }


{

parserError :: [Token] -> a
parserError _ = error "Erro sintático: Sequência de instruções inválida!"

}