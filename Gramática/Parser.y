{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parserError }

%token 
    num    { TokenNum $$ }
    var    { TokenVar $$ }
    '+'    { TokenPlus }
    '*'    { TokenTimes }
    '&'    { TokenAnd }
    '|'    { TokenOr }
    '{'    { TokenLBracket }
    '='    { TokenAssign } 
    '}'    { TokenRBracket }
    let    { TokenLet }
    in     { TokenIn }


%nonassoc if then else
%left '+'
%left '*' 

%%

Exp : num         { Num $1 }
    | var         { Var $1 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '*' Exp { Times $1 $3 }
    | Exp '&' Exp { And $1 $3 }
    | Exp '|' Exp { Or $1 $3 }
    | let var '=' Exp in Exp { Let $2 $4 $6}
    | '{' var '=' Exp '}' { Record $2 $4 }

{

-- data Expr = Num Int 
--           | Add Expr Expr  
--           | Times Expr Expr  
--           | Bracket Expr
--           | Let Expr  
--           deriving Show  

parserError :: [Token] -> a
parserError _ = error "Erro sintático: Sequência de instruções inválida!"

}