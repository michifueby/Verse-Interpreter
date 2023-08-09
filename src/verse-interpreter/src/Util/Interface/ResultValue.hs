
{-
    Project:    Verse-Interpreter
    File:       ResultValue.hs
-}


module Util.Interface.ResultValue where

import qualified Util.Datatypes.Types as T
import qualified Util.Interface.AbstractSyntaxTree as Ast
import qualified Data.Map as Map

import Util.Shared


{- 
    -------------------------------------------
    |              Result Type                |                
    -------------------------------------------
-}

{-
    Represents the Structure.
-}
type Structure = Ast.VerseExp

{-
    Represents the Result type for the evaluate function.
-}
data Result = Int Int 
            | Type T.Type
            | Fail
            | Structure Structure
            | Nothing
            | String String 
            | CustomType Name [(Name, Result)]
            | Var Name 
            | Tuple [Result]
            | Choice [Result]
            | AllResult [Result]
            | IntCollection [Int]
    deriving (Show, Eq, Ord) 

toChoice :: Result -> Result
toChoice (Tuple t) = Choice t
toChoice (Choice t) = Choice t
toChoice (IntCollection t) = Choice (map Int t)
toChoice _ = error "Invalid Argument provided"

{-
    Gets the Tuple from the Result.
-}
toTuple :: Result -> Result
toTuple (Choice t) = Tuple t
toTuple (Tuple t) = Tuple t
toTuple (IntCollection t) = Tuple (map Int t)
toTuple _ = error "Invalid Argument provided"

{-
    Gets the Int from the Result.
-}
toInt :: Result -> Int
toInt (Int n) = n
toInt _ = error "Invalid Argument provided"

{-
    Gets the String from the Result.
-}
toString :: Result -> String
toString (Int n) = show n
toString (String s) = s

{-
    Extract the [Int] from Result.
-}
extractIntFromCollection :: Result -> [Int]
extractIntFromCollection (Choice t) = disassemble t
    where 
        disassemble [] = []
        disassemble (Int x:xs) = x : disassemble xs
        disassemble (x:xs) = disassemble xs
extractIntFromCollection (Tuple t) = disassemble t
    where 
        disassemble [] = []
        disassemble (Int x:xs) = x : disassemble xs
        disassemble (x:xs) = disassemble xs


