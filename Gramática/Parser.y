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
    true   { TokenBTrue }
    false  { TokenBFalse }
    '+'    { TokenPlus }
    '*'    { TokenTimes }
    '&'    { TokenAnd }
    '|'    { TokenOr }
    '{'    { TokenLBracket }
    '='    { TokenAssign } 
    ','    { TokenComma } 
    '}'    { TokenRBracket }
    let    { TokenLet }
    in     { TokenIn }


%nonassoc if then else
%left '+'
%left '*' 

%%

Exp : num         { Num $1 }
    | var         { Var $1 }
    | true        { BTrue }
    | false       { BFalse }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '*' Exp { Times $1 $3 }
    | Exp '&' Exp { And $1 $3 }
    | Exp '|' Exp { Or $1 $3 }
    | let var '=' Exp in Exp { Let $2 $4 $6}
    | '{' recordList '}' { recordList $2 }

recordList : '{' var '=' Exp '}' { [Record $2 $4] }
           | '{' var '=' Exp ',' Exp '}' { ([Record $2 $4]) : recordList $6 }


{

parserError :: [Token] -> a
parserError _ = error "Erro sintático: Sequência de instruções inválida!"

}