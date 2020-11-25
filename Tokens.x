{ 
module Tokens where 
}

%wrapper "posn"     -- Defines the wrapper being used, in this case posn allows us to get the position of the source token
$digit = 0-9        -- Defines the digit type, used to represent numbers
$alpha = [a-zA-Z]   -- Defines the alpha type, used to represent words   

-- Defines the different tokens used in the language
tokens :-
  $white+                         ; 
  "--".*                          ; 
  Bool                            { tok (\p s -> TokenTypeBool p ) }
  Int		                          { tok (\p s -> TokenTypeInt p ) }
  Var                             { tok (\p s -> TokenTypeVar p ) }
  List                            { tok (\p s -> TokenTypeList p ) }
  true		                        { tok (\p s -> TokenTrue p ) }
  false		                        { tok (\p s -> TokenFalse p ) }
  if		                          { tok (\p s -> TokenIf p ) }
  else		                        { tok (\p s -> TokenElse p ) }
  then		                        { tok (\p s -> TokenThen p ) }
  while		                        { tok (\p s -> TokenWhile p ) }
  for                             { tok (\p s -> TokenFor p ) }
  to                              { tok (\p s -> TokenTo p ) }
  "{"		                          { tok (\p s -> TokenStart p ) }
  "}"		                          { tok (\p s -> TokenStop p ) }
  "=="                            { tok (\p s -> TokenIsEqual p ) }
  "!="                            { tok (\p s -> TokenNotEqual p ) }
  "<"                             { tok (\p s -> TokenLessThan p ) }
  "<="                            { tok (\p s -> TokenLessEqual p ) }
  ">="                            { tok (\p s -> TokenMoreEqual p ) }
  ">"                             { tok (\p s -> TokenMoreThan p ) }
  ";"                             { tok (\p s -> TokenEndLine p ) }
  "="                             { tok (\p s -> TokenConstDef p ) }
  "("                             { tok (\p s -> TokenLParen p ) }
  ")"                             { tok (\p s -> TokenRParen p ) }
  "+"                             { tok (\p s -> TokenPlus p ) }
  "-"                             { tok (\p s -> TokenMinus p ) }
  "*"                             { tok (\p s -> TokenTimes p ) }
  "/"                             { tok (\p s -> TokenDivide p ) }
  "%"                             { tok (\p s -> TokenMod p ) }
  "^"                             { tok (\p s -> TokenSquare p ) }
  \"                              { tok (\p s -> TokenString p ) }
  "++"                            { tok (\p s -> TokenAppend p ) }
  "["                             { tok (\p s -> TokenStartList p ) }
  "]"                             { tok (\p s -> TokenEndList p ) }
  ","                             { tok (\p s -> TokenComma p ) }
  "!!"                            { tok (\p s -> TokenGet p ) }
  "-*"                            { tok (\p s -> TokenCommentStart p ) }
  "*-"                            { tok (\p s -> TokenCommentEnd p ) }
  "&&"                            { tok (\p s -> TokenAnd p ) }
  "||"                            { tok (\p s -> TokenOr p ) }
  "!"                             { tok (\p s -> TokenNot p ) }
  continue                        { tok (\p s -> TokenContinue p ) }
  length                          { tok (\p s -> TokenLength p ) }
  head                            { tok (\p s -> TokenHead p ) }
  print                           { tok (\p s -> TokenPrint p ) } 
  \'                              { tok (\p s -> TokenString p ) }
  $digit+                         { tok (\p s -> TokenInt p (read s) ) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 

{
 -- Defines the helper function
tok f p s = f p s

 -- Defines the data type which defines the tokens
data Token = 
  TokenTypeBool AlexPosn   |
  TokenTypeInt AlexPosn    | 
  TokenTypeVar AlexPosn    |
  TokenTypeList AlexPosn   |
  TokenTrue AlexPosn       |
  TokenFalse AlexPosn      |
  TokenIf	AlexPosn         |
  TokenThen	AlexPosn       |
  TokenElse AlexPosn       |
  TokenWhile AlexPosn      |
  TokenFor AlexPosn        |
  TokenTo AlexPosn         |
  TokenStart AlexPosn      |
  TokenStop AlexPosn       |
  TokenIsEqual AlexPosn    |
  TokenNotEqual AlexPosn   |
  TokenLessThan AlexPosn   |
  TokenLessEqual AlexPosn  |
  TokenMoreEqual AlexPosn  |
  TokenMoreThan AlexPosn   |
  TokenEndLine AlexPosn    |
  TokenConstDef AlexPosn   |
  TokenLParen AlexPosn     |
  TokenRParen AlexPosn     |
  TokenPlus AlexPosn       |
  TokenMinus AlexPosn      |
  TokenTimes AlexPosn      |
  TokenDivide AlexPosn     |
  TokenMod AlexPosn        |
  TokenSquare AlexPosn     |
  TokenString AlexPosn     |
  TokenAppend AlexPosn     | 
  TokenStartList AlexPosn  |
  TokenEndList AlexPosn    |
  TokenComma AlexPosn      |
  TokenGet AlexPosn        |
  TokenLength AlexPosn     | 
  TokenCommentStart AlexPosn |
  TokenCommentEnd AlexPosn |
  TokenAnd AlexPosn        |
  TokenOr AlexPosn         |
  TokenNot AlexPosn        | 
  TokenContinue AlexPosn   |
  TokenHead AlexPosn       |
  TokenPrint AlexPosn      |
  TokenWord AlexPosn       | 
  TokenInt AlexPosn Int    |
  TokenVar AlexPosn String
  deriving (Show)	  
  
 -- Defines the function which returns the location of a token in the program
tokenPosn :: Token -> String
tokenPosn (TokenTypeBool (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeVar (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeList (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFor (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTo (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStart (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStop (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIsEqual (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEqual (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessEqual (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreEqual (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreThan (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLine (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConstDef (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMod (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSquare (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAppend (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStartList (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndList (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGet (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCommentStart (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCommentEnd (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContinue (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHead (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWord (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
}