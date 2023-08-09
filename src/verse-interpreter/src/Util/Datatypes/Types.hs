{-
    Project:    Verse-Interpreter
    File:       Types.hs
-}

module Util.Datatypes.Types where
import Util.Shared ( Name )


{- 
    -------------------------------------------
    |                 Types                   |
    -------------------------------------------
-}

{-
    Represents the Type.
-}
data Type = Int
    | String 
    | Tuple   
    | TupleFixed [Type] 
    | Func
    | CustomType Name
    | Any
    deriving (Eq, Ord)

{-
    Create instance - Show for Type.
-}
instance Show Type where 
    show Int = "#Int"
    show String = "#String"
    show Tuple = "#Tuple"
    show (TupleFixed t) = "#Tuple: " ++ show t
    show Func = "#Func"
    show (CustomType tName) = "#" ++ tName
    show Any = "#Any"
