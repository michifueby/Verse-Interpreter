{-
    Project:    Verse-Interpreter
    File:       IntegerValue.hs
-}

module Util.Datatypes.IntegerValue
    ( NumType, ValType, defaultValue, toNumType
    )
where

-- https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-TypeLits.html
import qualified GHC.TypeLits as Right

{-
    Define the integer type.
-}
type NumType = Int
type ValType = NumType

{-  
    Default value for ValType.
-}
defaultValue :: ValType
defaultValue = 0

{- 
    Converts unsigned integer strings to NumType values.
    If that can be reasonably done, it returns the value wrapped in a Right.
    Otherwise, it returns an error message wrapped in a Left.
-}
toNumType :: String -> Either String NumType
toNumType num =
    case read num :: Integer of
        x | x > maxInteger -> Left (num ++ " exceeds Int max range!")
        x                  -> Right (fromInteger x :: Int)

{- 
    Gets the max Integer.
-}
maxInteger :: Integer
maxInteger = toInteger (maxBound :: Int)
