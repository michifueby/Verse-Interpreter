
{-
    Project:    Verse-Interpreter
    File:       OutputHandler.hs
-}

module Util.InputOutput.OutputHandler
    ( printText, printError, printResult
    ) where

import Util.Interface.ResultValue
import Util.Utility (isSequence)

printResult :: Result -> IO()
printResult (Int n) = printText (show n)
printResult (String s) = printText s
printResult (Tuple t) = printText (buildTuple (extractIntFromCollection (Tuple t)))
printResult (Choice c) = printText (buildChoice (extractIntFromCollection (Choice c)))


-- [1,2,3] --> 1 | [2,3]
-- 1 | [2,3] --> 1 | 2 | [3]
-- 1 | 2 | [3] -- 1 | 2 | 3
-- 1 | 2 | 3
buildChoice :: [Int] -> String
buildChoice [e] = show e  
buildChoice (x:xs)
    | ((isSequence (x:xs)) && ((length (x:xs) >= 3))) = (show (head (x:xs)) ++ ".." ++ show (last (x:xs)))
    | otherwise = show x ++ "|" ++ (buildChoice xs)

-- [] --> ()
-- [1,2,3] --> (1, 2, 3)
buildTuple :: [Int] -> String
buildTuple [] = "()"
buildTuple (x:xs) = "(" ++ show x ++ construct xs
    where 
        construct [] = ")"
        construct (x:xs) = ", " ++ show x ++ construct xs

{-
    Print text to console.
-}
printText :: String -> IO ()
printText = putStrLn

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