module Evaluator where
import Grammar
import System.IO
import Control.Exception

{- Definitons of data types taken from Grammar
data Type = TyInt | TyBool | TyVar | TyList Type

data Expr = TInt Int | TTrue | TFalse | TIf Expr Expr Expr | TWhile Expr Expr | TFor Expr Expr Expr | TEqual Expr Expr | TNEqual Expr Expr | TLessThan Expr Expr | TLessEqual Expr Expr |
            TMoreEqual Expr Expr | TMoreThan Expr Expr | TVar String | TProgram Expr Expr | TConstantDef Expr Expr | TPlus Expr Expr | TMinus Expr Expr | TTimes Expr Expr | 
            TDivide Expr Expr | TMod Expr Expr | TSquare Expr Expr | TAppend Expr Expr | TList Expr | TInnerList Expr Expr | TGet Expr Expr | TLength Expr | TContinue | TPrint Expr |
            TOutput String | TAnd Expr Expr | TOr Expr Expr | TNot Expr | THead Expr 

type TyEnv = [(String, Type)]
type ExEnv = [(String, Expr)]
-}

-- Evaluates expressions to their base values
eval :: ExEnv -> Expr -> Expr
eval _ (TInt num) = TInt num
eval _ (TTrue) = TTrue
eval _ (TFalse) = TFalse

-- Checks a given string doesn't equate to the name of a variable, if it does then return the variable's value instead
eval env (TVar var) | getExprFromEnv env var == Nothing = TVar var
                    | otherwise                         = removeJustExpr (getExprFromEnv env var )

-- Defines loops (which repeat until a condition is met) and if statements
eval env (TIf e1 e2 e3) | eval env e1 == TTrue = eval env e2
                        | otherwise            = eval env e3
eval env (TWhile e1 e2) | (eval env e1) == TFalse = eval env e2
                        | otherwise               = TProgram (eval env e2) (eval (checkUpdate env [e2]) (TWhile e1 e2))
eval env (TFor e1 num e2) | eval env (TEqual (eval env e1) num) == TTrue     = eval env e2
                          | eval env (TMoreThan (eval env e1) num) == TTrue  = TFalse
                          | otherwise                                        = TProgram (eval env e2) (eval (checkUpdate env [e2,num]) (TFor e1 num e2) )

-- Defines boolean operators that check their types so can return an error if given the wrong types, otherwise return a boolean value (TTrue or TFalse)
eval env (TEqual e1 e2) | eval env e1 == eval env e2 = TTrue
                        | otherwise                  = TFalse
eval env (TNEqual e1 e2) | eval env e1 == eval env e2 = TFalse
                         | otherwise                  = TTrue
eval env (TLessThan (TInt n1) (TInt n2)) | n1 < n2   = TTrue
                                         | otherwise = TFalse
eval env (TLessThan e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TLessThan (eval env e1) (eval env e2) )
                           | otherwise                                                                        = error "Invalid data type(s) for (<)"
eval env (TLessEqual (TInt n1) (TInt n2)) | n1 <= n2   = TTrue
                                          | otherwise   = TFalse
eval env (TLessEqual e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TLessEqual (eval env e1) (eval env e2) )
                            | otherwise                                                                        = error "Invalid data type(s) for (<=)"
eval env (TMoreEqual (TInt n1) (TInt n2)) | n1 >= n2   = TTrue
                                          | otherwise  = TFalse
eval env (TMoreEqual e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TMoreEqual (eval env e1) (eval env e2) )
                            | otherwise                                                                        = error "Invalid data type(s) for (>=)"
eval env (TMoreThan (TInt n1) (TInt n2)) | n1 > n2   = TTrue
                                         | otherwise = TFalse
eval env (TMoreThan e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TMoreThan (eval env e1) (eval env e2) )
                           | otherwise                                                                        = error "Invalid data type(s) for (>)"
eval env (TAnd e1 e2) | typeOf [] (eval env e1) == Right TyBool && typeOf [] (eval env e2) == Right TyBool = if ((eval env e1) == TTrue) && ((eval env e2) == TTrue) then TTrue else TFalse
                      | otherwise                                                              = error "Invalid data type(s) for (&&)"
eval env (TOr e1 e2) | typeOf [] (eval env e1) == Right TyBool && typeOf [] (eval env e2) == Right TyBool = if ((eval env e1) == TTrue) || ((eval env e2) == TTrue) then TTrue else TFalse
                     | otherwise                                                             = error "Invalid data type(s) for (||)"
eval env (TNot e) | typeOf [] (eval env e) == Right TyBool = if (eval env e) == TTrue then TFalse else TTrue
                  | otherwise                        = error "Invalid data type(s) for (!)"

-- Defines basic mathematic operators that check they're given ints and evaluate, otherwise they return an error
eval env (TPlus (TInt n1) (TInt n2)) = TInt (n1+n2)
eval env (TPlus (TList (TInt x)) (TList (TInt y))) = eval env (TPlus (TInt x) (TInt y))
eval env (TPlus (TList (TInt x)) y) = eval env (TPlus (TInt x) y)
eval env (TPlus x (TList (TInt y))) = eval env (TPlus x (TInt y))
eval env (TPlus (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (+)"
eval env (TPlus (TList (TInnerList x y)) a) = error "Invalid data type(s) for (+)"
eval env (TPlus a (TList (TInnerList x y))) = error "Invalid data type(s) for (+)"
eval env (TPlus e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TPlus (eval env e1) (eval env e2) )
                       | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TPlus (eval env e1) (eval env e2) )
                       | otherwise                                                                        = error "Invalid data type(s) for (+)"
eval env (TMinus (TInt n1) (TInt n2)) = TInt (n1-n2)
eval env (TMinus (TList (TInt x)) (TList (TInt y))) = eval env (TMinus (TInt x) (TInt y))
eval env (TMinus (TList (TInt x)) y) = eval env (TMinus (TInt x) y)
eval env (TMinus x (TList (TInt y))) = eval env (TMinus x (TInt y))
eval env (TMinus (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (-)"
eval env (TMinus (TList (TInnerList x y)) a) = error "Invalid data type(s) for (-)"
eval env (TMinus a (TList (TInnerList x y))) = error "Invalid data type(s) for (-)"
eval env (TMinus e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TMinus (eval env e1) (eval env e2) )
                        | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TMinus (eval env e1) (eval env e2) )
                        | otherwise                                                                        = error "Invalid data type(s) for (-)"
eval env (TTimes (TInt n1) (TInt n2)) = TInt (n1*n2)
eval env (TTimes (TList (TInt x)) (TList (TInt y))) = eval env (TTimes (TInt x) (TInt y))
eval env (TTimes (TList (TInt x)) y) = eval env (TTimes (TInt x) y)
eval env (TTimes x (TList (TInt y))) = eval env (TTimes x (TInt y))
eval env (TTimes (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (*)"
eval env (TTimes (TList (TInnerList x y)) a) = error "Invalid data type(s) for (*)"
eval env (TTimes a (TList (TInnerList x y))) = error "Invalid data type(s) for (*)"
eval env (TTimes e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TTimes (eval env e1) (eval env e2) )
                        | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TTimes (eval env e1) (eval env e2) )
                        | otherwise                                                                        = error "Invalid data type(s) for (*)"
eval env (TDivide (TInt n1) (TInt n2)) = TInt (n1 `div` n2)
eval env (TDivide (TList (TInt x)) (TList (TInt y))) = eval env (TDivide (TInt x) (TInt y))
eval env (TDivide (TList (TInt x)) y) = eval env (TDivide (TInt x) y)
eval env (TDivide x (TList (TInt y))) = eval env (TDivide x (TInt y))
eval env (TDivide (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (/)"
eval env (TDivide (TList (TInnerList x y)) a) = error "Invalid data type(s) for (/)"
eval env (TDivide a (TList (TInnerList x y))) = error "Invalid data type(s) for (/)"
eval env (TDivide e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TDivide (eval env e1) (eval env e2) )
                         | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TDivide (eval env e1) (eval env e2) )
                         | otherwise                                                                         = error "Invalid data type(s) for (/)"
eval env (TMod (TInt n1) (TInt n2)) = TInt (n1 `mod` n2)
eval env (TMod (TList (TInt x)) (TList (TInt y))) = eval env (TMod (TInt x) (TInt y))
eval env (TMod (TList (TInt x)) y) = eval env (TMod (TInt x) y)
eval env (TMod x (TList (TInt y))) = eval env (TMod x (TInt y))
eval env (TMod (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (%)"
eval env (TMod (TList (TInnerList x y)) a) = error "Invalid data type(s) for (%)"
eval env (TMod a (TList (TInnerList x y))) = error "Invalid data type(s) for (%)"
eval env (TMod e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TMod (eval env e1) (eval env e2) )
                      | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TMod (eval env e1) (eval env e2) )
                      | otherwise                                                                        = error "Invalid data type(s) for (%)"
eval env (TSquare (TInt n1) (TInt n2)) = TInt (n1^n2)
eval env (TSquare (TList (TInt x)) (TList (TInt y))) = eval env (TSquare (TInt x) (TInt y))
eval env (TSquare (TList (TInt x)) y) = eval env (TSquare (TInt x) y)
eval env (TSquare x (TList (TInt y))) = eval env (TSquare x (TInt y))
eval env (TSquare (TList (TInnerList x y)) (TInnerList a b)) = error "Invalid data type(s) for (^)"
eval env (TSquare (TList (TInnerList x y)) a) = error "Invalid data type(s) for (^)"
eval env (TSquare a (TList (TInnerList x y))) = error "Invalid data type(s) for (^)"
eval env (TSquare e1 e2) | typeOf [] (eval env e1) == Right TyInt && typeOf [] (eval env e2) == Right TyInt = eval env (TSquare (eval env e1) (eval env e2) )
                         | typeOf [] (eval env e1) == Right (TyList TyInt) || typeOf [] (eval env e2) == Right (TyList TyInt) = eval env (TSquare (eval env e1) (eval env e2) )
                         | otherwise                                                                        = error "Invalid data type(s) for (^)"

-- Evaluation for the append operators, uses a different function to ensure the correct data types are used
eval env (TAppend (TList e1) e2) = append (TList e1) (eval env e2)
eval env (TAppend e1 e2 ) = append (eval env e1) (eval env e2)

-- Evaluation for list creation, also fixes some pattern issues with input files
eval env (TList (TProgram e1 e2)) = error "Invalid data type for (List)"
eval env (TList (TList x)) = (eval env (TList x))
eval env (TList (TInnerList e1 e2)) = TList (eval env (TInnerList e1 e2) )
eval env (TList e) = TList e
eval env (TInnerList e1 e2) | typeOf [] (eval env e1) /= typeOf [] (eval env e2) = error "List data must be of the same type"
                            | otherwise                                          = TInnerList (eval env e1) (eval env e2)

-- Evaluation for list functions get and length (checks types to ensure only lists can be used)
eval env (TGet list (TInt n)) | (listType == Right (TyList TyInt) ) || (listType == Right (TyList TyBool) ) || (listType == Right (TyList TyVar) ) = getN env (eval env list) (eval env (TInt n)) 0
                              | (n>1) && ((listType == Right (TyList (TyList TyInt))) || (listType == Right (TyList (TyList TyBool))) 
                                || listType == Right (TyList (TyList TyVar)) )                                                              = TList (getN env (eval env list) (TInt n) 0)
                              | (n==1) && ((listType == Right (TyList (TyList TyInt))) || (listType == Right (TyList (TyList TyBool))) 
                                || listType == Right (TyList (TyList TyVar)) )                                                              = getN env (eval env list) (TInt n) 0
                              | otherwise                                                                                                   = error "Invalid data type(s) for (!!)"
 where listType = typeOf [] (eval env list)
eval env (TGet list n) | typeOf [] (eval env n) /= Right TyInt = error "Invalid data type(s) for (!!)"
                       | otherwise                             = eval env (TGet list (eval env n))
eval env (TLength (TContinue)) = TInt 0
eval env (TLength (TList TContinue)) = TInt 0
eval env (TLength e) | lType == Right (TyList TyVar) || lType == Right (TyList TyInt) || lType == Right (TyList TyBool) = getLength (eval env e) 0
                     | otherwise                                                                                    = error "Invalid data type(s) for (length)"
 where lType = typeOf [] (eval env e)

-- Defines evaluation function for TPrint which is used to output values, returns a TOuput expression which uses strings for outputs
eval env (TPrint e) = TOutput (outputVar env e)

-- Defines the program (multiple statements) and constantdef (used when defining a new variable)
eval env (TProgram e1 e2) = TProgram (eval env e1) (eval (checkUpdate env [e1]) e2)
eval env (TConstantDef (TVar constName) constValue) = eval env (eval env constValue)

-- A catch if any expressions that aren't recognised are used
eval _ _ = TContinue

-- Puts a list in a readable form for outputting
outputList :: ExEnv -> Expr -> String
outputList env (TList (TInnerList x y)) = "[ " ++ outputList env (TInnerList x y)
outputList env (TList x) = (outputVar env x)
outputList env (TInnerList x (TInnerList y z)) = (outputVar env x) ++ ", " ++ (outputList env (TInnerList y z))
outputList env (TInnerList x y) = (outputVar env x) ++ ", " ++ (outputVar env y) ++ " ]"

-- Makes a given value readable and converts to a string, for outputs
outputVar :: ExEnv -> Expr -> String
outputVar env (TInt x) = show x
outputVar env (TVar x) | getExprFromEnv env x == Nothing = show x
                       | otherwise                       = outputVar env (eval env (TVar x))
outputVar env (TList x) = outputList env (TList x)
outputVar env e = outputVar env (eval env e)

-- Method to get the length of a list
getLength :: Expr -> Int -> Expr
getLength (TList (TContinue)) _ = (TInt 0)
getLength (TList e) n = getLength e (n+1)
getLength (TInnerList e1 e2) n = getLength e2 (n+1)
getLength _ n = TInt n

-- Method to get nth value of a list
getN :: ExEnv -> Expr -> Expr -> Int -> Expr
getN _ (TList (TContinue)) _ _ = TList TContinue
getN env (TList (TList e)) (TInt x) n = getN env (TList e) (TInt x) n
getN env (TList (TInnerList e1 e2)) (TInt x) n | x == n    = TList (TInnerList e1 e2)
                                               | otherwise = getN env (TInnerList e1 e2) (TInt x) (n+1)
getN env (TList e) (TInt x) n | x == n    = e
                              | x == 1    = e
                              | otherwise = getN env e (TInt x) (n+1)
getN env (TInnerList e1 e2) (TInt x) n | x == n     = e1
                                       | x < n      = error "Array Out of Bounds"
                                       | otherwise  = getN env e2 (TInt x) (n+1)
getN env (TInt e) (TInt x) n | x == n    = TInt e
                             | otherwise = error "Array Out of Bounds"
getN _ _ _ _ = TFalse

-- Method for appending variables to a string/list, checks the values are the same type
append :: Expr -> Expr -> Expr
append (TVar prefix) (TVar suffix) = TVar (prefix ++ suffix)
append (TList (TContinue)) (TList suffix) = TList suffix
append (TList prefix) (TList suffix) = append (TList prefix) suffix
append (TList (TContinue)) suffix = TList suffix
append (TList prefix) suffix | listType == typeOf [] suffix = newList (TList prefix) suffix
                             | listType == typeOf [] suffix = newList (TList prefix) suffix
                             | listType == typeOf [] suffix = newList (TList prefix) suffix
                             | otherwise                    = error "Must be same type for (++)" 
 where listType = typeOf [] prefix
append _ _ = error "Wrong data type for (++)"

-- Creates a new list with the appended value added on
newList :: Expr -> Expr -> Expr
newList (TList e) suffix = TList (newList e suffix)
newList (TInnerList e1 e2) suffix = TInnerList e1 (newList e2 suffix)
newList prefix suffix = TInnerList prefix suffix

-- Checks if the environment will be updated with a new/changed variable value and changes if so
checkUpdate :: ExEnv -> [Expr] -> ExEnv
checkUpdate env ((TProgram e1 e2):xs ) = checkUpdate (checkUpdate env [e1,e2]) xs
checkUpdate env ((TConstantDef (TVar name) val):xs ) = checkUpdate (addExpr name val env) xs
checkUpdate env ((TIf e1 e2 e3):xs) | (eval env e1) == TTrue = checkUpdate (checkUpdate env [e2]) xs
                                    | otherwise              = checkUpdate (checkUpdate env [e3]) xs
checkUpdate env ((TFor e1 e2 e3):xs) | eval env (TMoreEqual (eval env e1) (eval env e2)) == TTrue = env ++ checkUpdate env xs
                                     | otherwise                      = checkUpdate (checkUpdate env [e3]) [(TFor e1 e2 e3)]
checkUpdate env (x:xs) = checkUpdate env xs
checkUpdate env [] = env

-- Returns the type of the given expression or an error if can't be found
typeOf :: TyEnv -> Expr -> Either String Type
typeOf _ (TInt _) = Right TyInt
typeOf _ (TTrue) = Right TyBool
typeOf _ (TFalse) = Right TyBool
typeOf _ (TEqual _ _) = Right TyBool
typeOf _ (TNEqual _ _) = Right TyBool
typeOf _ (TLessThan _ _) = Right TyBool
typeOf _ (TLessEqual _ _) = Right TyBool
typeOf _ (TMoreEqual _ _) = Right TyBool
typeOf _ (TMoreThan _ _) = Right TyBool
typeOf env (TPlus _ _) = Right TyInt
typeOf env (TMinus _ _) = Right TyInt
typeOf env (TTimes _ _) = Right TyInt
typeOf env (TDivide _ _) = Right TyInt
typeOf env (TMod _ _) = Right TyInt
typeOf env (TSquare _ _) = Right TyInt
typeOf _ (TAppend _ _) = Right TyVar
typeOf env (TGet x y) = Right TyInt
typeOf env (TVar var) | getTypeFromEnv env var == Nothing = Right TyVar
                      | otherwise                         = Right (removeJustType (getTypeFromEnv env var ) )
typeOf env (TIf e1 e2 e3) | (typeOf env e1 == Right TyBool) && (typeOf env e2 == typeOf env e3 ) = typeOf env e2
                          | otherwise                                                            = Left "Mismatched Types"
typeOf env (TWhile e1 e2) | typeOf env e1 == Right TyBool = typeOf env e2
                          | otherwise                     = Left "Mismatched Types"
typeOf env (TInnerList e1 e2) = typeOf env e1 
typeOf env (TList (TInnerList (TList a) (TList b))) | typeOf env a == typeOf env b = Right (TyList (removeRight (typeOf env (TList a) ) ) )
                                                    | otherwise                    = Left "Mismatched Types"
typeOf env (TList (TContinue)) = Right (TyList TyBool)
typeOf env (TList e) = Right (TyList (removeRight (typeOf env e) ) )

--typeOf env (TInnerList (TList a) (TList b)) | typeOf env a == typeOf env b = Right 
typeOf env (TConstantDef constName constValue) = typeOf env constValue
typeOf env (TProgram e1 e2) | (typeOf env e1) == (typeOf env e2) = typeOf env e1
                            | otherwise                          = Left "Mismatched Types"
typeOf _ _ = Left "Unknown Type"

-- Method to remove "Right" from the either expression when returning a type
removeRight :: Either String Type -> Type
removeRight (Right x) = x

-- Adds a new type to the environment
addType :: String -> Type -> TyEnv -> TyEnv
addType varname ty env = (varname, ty) : env

-- Returns the type of a given variable name from the environment
getTypeFromEnv :: TyEnv -> String -> Maybe Type
getTypeFromEnv [] _ = Nothing
getTypeFromEnv ( (tyVarname, ty) : env ) varname | varname == tyVarname = Just ty
                                                 | otherwise            = getTypeFromEnv env varname

-- Adds a given variable to the environment with the name of it and the Expr value
addExpr :: String -> Expr -> ExEnv -> ExEnv
addExpr varname exp env = (varname, (eval env exp) ) : env

-- Returns the Expr value of a variable name from the environment
getExprFromEnv :: ExEnv -> String -> Maybe Expr
getExprFromEnv [] _ = Nothing
getExprFromEnv ( (exVarname, exp) : env ) varname | varname == exVarname = Just exp
                                                  | otherwise            = getExprFromEnv env varname

-- Removes the 'Just' from the Maybe Expr value
removeJustExpr :: Maybe Expr -> Expr
removeJustExpr (Just ex) = ex

-- Removes the 'Just' from the Maybe Type value
removeJustType :: Maybe Type -> Type
removeJustType (Just ty) = ty 
