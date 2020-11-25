{ 
module Grammar where 
import Tokens
}

%name parseProgram          -- Defines the name of the generated parse function
%tokentype { Token }        -- Defines the type of the Tokens
%error { parseError }       -- Defines the name of the function to call if an error occurs

-- Gives names of values seen in the input stream
%token 
    Bool        { TokenTypeBool _ }
    Int         { TokenTypeInt _ }
    Var         { TokenTypeVar _ }
    List        { TokenTypeList _ }
    true        { TokenTrue _ }
    false       { TokenFalse _ }
    int         { TokenInt _ $$ }
    if          { TokenIf _ }
    then        { TokenThen _ }
    else        { TokenElse _ }
    while       { TokenWhile _ }
    for         { TokenFor _ }
    to          { TokenTo _ }
    '{'         { TokenStart _ }
    '}'         { TokenStop _ }
    '=='        { TokenIsEqual _ }
    '!='        { TokenNotEqual _ }
    '<'         { TokenLessThan _ }
    '<='        { TokenLessEqual _ }
    '>'         { TokenMoreThan _ }
    '>='        { TokenMoreEqual _ }
    ';'         { TokenEndLine _ }
    '='         { TokenConstDef _ }
    '('         { TokenLParen _ }
    ')'         { TokenRParen _ }
    '+'         { TokenPlus _ }
    '-'         { TokenMinus _ }
    '*'         { TokenTimes _ }
    '/'         { TokenDivide _ }
    '%'         { TokenMod _ }
    '^'         { TokenSquare _ }
    '"'         { TokenString _ }
    '++'        { TokenAppend _ }
    '['         { TokenStartList _ }
    ']'         { TokenEndList _ }
    ','         { TokenComma _ }
    '!!'        { TokenGet _ }
    '-*'        { TokenCommentStart _ }
    '*-'        { TokenCommentEnd _ }
    '&&'        { TokenAnd _ }
    '||'        { TokenOr _ }
    '!'         { TokenNot _ }
    length      { TokenLength _ }
    continue    { TokenContinue _ }
    head        { TokenHead _ }
    print       { TokenPrint _ }
    "'"         { TokenWord _ }
    var         { TokenVar _ $$ }

-- Defines the associativity of the Tokens
%nonassoc if
%nonassoc then
%nonassoc else
%nonassoc while
%nonassoc for
%nonassoc '{' '}' '"'
%nonassoc int true false '(' ')' '#FILE#'
%left '=' '+' '-' '*' '/' '%' '<' '++'
%left APP

-- Defines the productions of the grammar
%%
Exp : int                                           { TInt $1 }
    | var                                           { TVar $1 }
    | true                                          { TTrue }
    | false                                         { TFalse }
    | if Exp '{' Exp '}' else '{' Exp '}'';'        { TIf $2 $4 $8 }
    | while Exp '{' Exp '}' ';'                     { TWhile $2 $4 }
    | for Exp to Exp '{' Exp '}' ';'                 { TFor $2 $4 $6 }
    | Exp '==' Exp                                  { TEqual $1 $3 }
    | Exp '!=' Exp                                  { TNEqual $1 $3 }
    | Exp '<' Exp                                   { TLessThan $1 $3 }
    | Exp '<=' Exp                                  { TLessEqual $1 $3 }
    | Exp '>=' Exp                                  { TMoreEqual $1 $3 }
    | Exp '>' Exp                                   { TMoreThan $1 $3 }
    | '(' Exp ')'                                   { $2 }
    | Exp '+' Exp                                   { TPlus $1 $3 }
    | Exp '-' Exp                                   { TMinus $1 $3 }
    | '-' int                                       { TMinus (TInt 0) (TInt $2) }
    | Exp '*' Exp                                   { TTimes $1 $3 }
    | Exp '/' Exp                                   { TDivide $1 $3 }
    | Exp '%' Exp                                   { TMod $1 $3 }
    | Exp '^' Exp                                   { TSquare $1 $3 }
    | '"' Exp '"'                                   { $2 }
    | Exp '++' Exp                                  { TAppend $1 $3 }
    | var '=' Exp ';'                               { TConstantDef (TVar $1) $3 }
    | '[' Exp ']'                                   { TList $2 }
    | '[' ']'                                       { TList TContinue }
    | Exp ',' Exp                                   { TInnerList $1 $3 }
    | Exp '!!' Exp                                  { TGet $1 $3 }
    | '-*' Exp '*-'                                 { TContinue }
    | length Exp                                    { TLength $2 }
    | head Exp                                      { THead $2 }
    | Exp '&&' Exp                                  { TAnd $1 $3 }
    | Exp '||' Exp                                  { TOr $1 $3 }
    | '!' Exp                                       { TNot $2 }
    | continue ';'                                  { TContinue }
    | print Exp ';'                                 { TPrint $2 }
    | "'" Exp "'"                                   { TVar (show $2) }
    | Exp Exp %prec APP                             { TProgram $1 $2 }


Type : Int      { TyInt }
     | Bool     { TyBool }
     | Var      { TyVar }

-- Post-Amble: Defines the Haskell code that's copied directly to the output
{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Type = TyInt | TyBool | TyVar | TyList Type
            deriving (Show, Eq)

data Expr = TInt Int | TTrue | TFalse | TIf Expr Expr Expr | TWhile Expr Expr | TFor Expr Expr Expr | TEqual Expr Expr | TNEqual Expr Expr | TLessThan Expr Expr | TLessEqual Expr Expr |
            TMoreEqual Expr Expr | TMoreThan Expr Expr | TVar String | TProgram Expr Expr | TConstantDef Expr Expr | TPlus Expr Expr | TMinus Expr Expr | TTimes Expr Expr | 
            TDivide Expr Expr | TMod Expr Expr | TSquare Expr Expr | TAppend Expr Expr | TList Expr | TInnerList Expr Expr | TGet Expr Expr | TLength Expr | TContinue | TPrint Expr |
            TOutput String | TAnd Expr Expr | TOr Expr Expr | TNot Expr | THead Expr 
           deriving (Show, Eq)

type TyEnv = [(String, Type)]
type ExEnv = [(String, Expr)]
}