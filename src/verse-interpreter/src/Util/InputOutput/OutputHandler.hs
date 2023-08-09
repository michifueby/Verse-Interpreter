
{-
    Project:    Verse-Interpreter
    File:       OutputHandler.hs
-}

module Util.InputOutput.OutputHandler
    ( printText, printError, printResult
    ) where

import qualified Util.Datatypes.Types as T

import Util.Interface.ResultValue  as R
import Util.Utility
import Data.List (intercalate)
import Util.Interface.AbstractSyntaxTree (VerseExp)


{- 
    -------------------------------------------
    |             Output Handler              |                
    -------------------------------------------
-}

{-
    Prints the Result from evaluator.
-}
printResult :: Result -> IO()
printResult R.Nothing = printText ""
printResult R.Fail = printText "fail"
printResult (Int n) = printText (show n)
printResult (String s) = printText s
printResult (Structure s) = printText $ show s
printResult (AllResult r) = printText $ "{ " ++ display (flattern r) ++ " }"
        where
            flattern :: [Result] -> [Result] 
            flattern [] = []
            flattern (R.Nothing:xs) = flattern xs
            flattern (R.AllResult r:xs) = flattern r ++ flattern xs 
            flattern (R.Tuple r:xs) = flattern r ++ flattern xs 
            flattern (R.Choice r:xs) = flattern r ++ flattern xs 
            flattern (x:xs) = x : flattern xs
            display :: [Result] -> [Char]
            display [] = [] 
            display (R.String s:xs) = s ++ "; " ++ display xs 
            display (R.CustomType name fields:xs) = buildCustomType (R.CustomType name fields) ++ "; " ++ display xs 
            display (R.Fail:xs) = "fail" ++ "; " ++ display xs 
            display (R.Var var:xs) = "Var " ++ var ++ "; " ++ display xs 
            display (R.Int i:xs) = show i ++ "; " ++ display xs 
            display (x:xs) = show x ++ "; " ++ display xs 

printResult (Var n) = printText n
printResult (Type t) = printText $ displayType $ Type t
printResult (Tuple t) = printText $ displayTuple $ Tuple t
printResult (IntCollection t) = printText $ displayTuple $ IntCollection t
printResult (Choice c) = printText $ displayChoice $ Choice c
printResult (CustomType name fieldList) = printText $ buildCustomType $ CustomType name fieldList

{-
    Display the tuple from Result as String.
-}
displayTuple :: Result -> String
displayTuple (Int n) = show n
displayTuple (String s) = show s
displayTuple R.Nothing = ""
displayTuple R.Fail = "fail"
displayTuple (Tuple []) = ""
displayTuple (Tuple [x]) = displayTuple x
displayTuple (Tuple xs) = "(" ++ intercalate "," (map displayTuple xs) ++ ")"

{-
    Display the Type as String.
-}
displayType :: Result -> String
displayType (Type T.Int) = "int"
displayType (Type T.String) = "string"
displayType (Type T.Tuple) = "tuple"
displayType (Type T.Any) = "any"

{-
    Display the choice from Result.
-}
displayChoice :: Result -> String
displayChoice (Int n) = show n
displayChoice (String s) = show s
displayChoice R.Nothing = ""
displayChoice R.Fail = "fail"
displayChoice (Tuple [x]) = displayTuple x
displayChoice (Choice []) = ""
displayChoice (Tuple xs) = "(" ++ intercalate "," (map displayTuple xs) ++ ")"
displayChoice (Choice [x]) = displayChoice x
displayChoice (Choice (x:xs))
    | isASequence (x:xs) && (length (x:xs) >= 3) = toString (head (x:xs)) ++ ".." ++ toString (last (x:xs))
    | otherwise = "(" ++ intercalate "|" (map displayChoice (x:xs)) ++ ")" 

{-
    Builds the custom type for the output.
-}
buildCustomType :: Result -> String
buildCustomType (CustomType name fieldList) = name ++ " { " ++ fields ++ " }"
  where
    fields :: [Char]
    fields = intercalate ", " $ map buildFields fieldList

{-
    Builds the fields from the custom type.
-}
buildFields :: (String, Result) -> String
buildFields (n, r) = n ++ ": " ++ toString r

{-
    Print text to console.
-}
printText :: String -> IO ()
printText text = putStrLn $ "> " ++ text ++ "\n"

{-
    Print error to console.
-}
printError :: [Char] -> a
printError = error

{-
    Print error with line number to console.
-}
printErrorWithLineNumber :: Show a => a -> [Char] -> IO ()
printErrorWithLineNumber lineNumber text = putStrLn (show lineNumber ++ text)