
{-
    Project:    Verse-Interpreter
    File:       ResultValue.hs
-}


module Util.Interface.ResultValue where

import Util.Shared
import qualified Util.Interface.AbstractSyntaxTree as Ast

import qualified Util.Datatypes.Types as T

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
            | Var Name 
            | Tuple [Result]
            | Choice [Result]
            | IntCollection [Int]
    deriving (Show, Eq, Ord) 

toChoice :: Result -> Result
toChoice (Tuple t) = Choice t
toChoice (Choice t) = Choice t
toChoice (IntCollection t) = Choice (map Int t)
toChoice _ = error "Invalid Argument provided"

toTuple :: Result -> Result
toTuple (Choice t) = Tuple t
toTuple (Tuple t) = Tuple t
toTuple (IntCollection t) = Tuple (map Int t)
toTuple _ = error "Invalid Argument provided"

toInt :: Result -> Int
toInt (Int n) = n
toInt _ = error "Invalid Argument provided"

extractIntFromCollection :: Result -> [Int]
extractIntFromCollection (Choice t) = disassemble t
    where 
        disassemble [] = []
        disassemble [x] = [toInt x] 
        disassemble (Int x:xs) = x : disassemble xs
extractIntFromCollection (Tuple t) = disassemble t
    where 
        disassemble [] = []
        disassemble [x] = [toInt x] 
        disassemble (Int x:xs) = x : disassemble xs

