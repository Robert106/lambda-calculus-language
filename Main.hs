import System.Environment (getArgs)
import Tokens
import Grammar
import Evaluator
import System.IO
import Control.Exception

main :: IO ()
main = catch main' noParse

main' = do { (scriptFile : _ ) <- getArgs 
           ; sourceScript <- readFile scriptFile
           ; sourceProblem <- getContents
           ; let parsedProb = parseProgram (alexScanTokens (show (makeColumns sourceProblem) ) )
           ; let parsedProg = parseProgram (alexScanTokens sourceScript)
           ; let typedProg = removeSide (typeOf [] parsedProg)
           ; let result = show (eval (checkUpdate [] [(TConstantDef (TVar "problem") (eval [] parsedProb) )] ) parsedProg )
           ; putStrLn (outputArr (checkCompleteEval (words result) ) ) } 

-- Called if there's a parse error
noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

-- Main function to seperate input file into columns to return a list of columns
makeColumns :: String -> [[String]]
makeColumns text | null text = [["[]"]]
                 | otherwise = filter (not . null) (getColumns ((getIndvLength colWords) - 1 ) 0 colWords )
 where colWords = filter (not . null) (getWords (lines text) ) --(getLines text) )--(lines text) )

-- Gets the individual words each line
getWords :: [String] -> [[String]]
getWords (x:[]) = [words x]
getWords (x:xs) = words x : getWords xs
getWords [] = [[]]

-- Gets the actual corrosponding columns from the split lines
getColumns :: Int -> Int -> [[String]] -> [[String]]
getColumns length currentGet fullList | length >= currentGet = (getIndvCol currentGet fullList) : (getColumns length (currentGet+1) fullList)
                                      | otherwise           = [[]]

-- Splits each individual line into columns
getIndvCol :: Int -> [[String]] -> [String]
getIndvCol getNo (xs:xss) = [(xs !! getNo)] ++ getIndvCol getNo xss
getIndvCol _ [] = []

-- Gets the length of a single line (the number of columns needed)
getIndvLength :: [[String]] -> Int
getIndvLength (xs:xss) = length xs
getIndvLength [] = 0

-- Checks the complete evaluation, outputting anything that needs to be
checkCompleteEval :: [String] -> [String]
checkCompleteEval (x:xs) | x == "TOutput" = output xs ++ checkCompleteEval xs
                         | x == "(TOutput" = output xs ++ checkCompleteEval xs
                         | otherwise      = checkCompleteEval xs
checkCompleteEval [] = []

-- Filters the output to be more readable
output :: [String] -> [String]
output (x:xs) | (head x) == '(' = output ((tail x):xs)
              | (last x) == ')' = output [(init x)]
              | x == "\""        = output xs
              | (head x) == '\"' = output ((tail x):xs)
              | (last x) == '\"' = output [(init x)]
              | x == "\\"        = output xs
              | (head x) == '\\' = output ((tail x):xs)
              | (last x) == '\\' = output [(init x)]
              | otherwise       = x : output xs
output [] = []

-- Outputs the array of TOutput values in a better way
outputArr :: [String] -> String
outputArr (x:[]) = x
outputArr (x:xs) = (x ++ "\n")++outputArr xs
outputArr [] = []

-- Removes the "Right" or "Left" from type-checked values
removeSide :: Either String Type -> String
removeSide (Right x) = show x
removeSide (Left x) = show x