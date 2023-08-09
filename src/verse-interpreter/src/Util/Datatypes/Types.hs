module Util.Datatypes.Types where

data Type = Int
    | Tuple   
    | TupleFixed [Type]  
    deriving (Eq, Ord)

instance Show Type where 
    show (Int) = "#Int"
    show (Tuple) = "#Tuple"
    show (TupleFixed t) = "#Tuple: " ++ show t