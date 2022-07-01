{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parserError }

%token 
    num    { TokenNum $$ }
    '+'    { TokenPlus }
    '*'    { TokenTimes }
    '('    { TokenLParen }
    ')'    { TokenRParen }
    if     { TokenIf }
    then   { TokenThen }
    else   { TokenElse }

%nonassoc if then else
%left '+'
%left '*' 

%%

Exp : num         { Num $1 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '*' Exp { Times $1 $3 }
    | '(' Exp ')' { Paren $2 }
    | if Exp then Exp else Exp { If $2 $4 $6 }

{

data Expr = Num Int 
          | Add Expr Expr  
          | Times Expr Expr  
          | Paren Expr  
          | If Expr Expr Expr
          deriving Show  

parserError :: [Token] -> a
parserError _ = error "Erro sintático: Sequência de instruções inválida!"

}