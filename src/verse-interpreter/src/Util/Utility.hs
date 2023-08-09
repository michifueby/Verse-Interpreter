{-
    Project:    Verse-Interpreter
    File:       Utility.hs
-}

module Util.Utility
    ( toAddApplicative, isSequence, isASequence
    ) where

import Data.Char
import Util.Interface.ResultValue


{- 
    -------------------------------------------
    |                 Utility                 |                
    -------------------------------------------
-}

{-
  Make to Add Applicative.
-}
toAddApplicative :: (Num a) => [[a]] -> [a]
toAddApplicative = foldl (\acc xs -> (+) <$> acc <*> xs) (pure 0)

{-
  Check if a is a sequence.
-}
isSequence :: (Num a, Eq a) => [a] -> Bool
isSequence [] = True
isSequence [_] = True
isSequence (this:other:rest)   
  | other == this + 1 || other == this - 1 = isSequence (other:rest)    
  | otherwise = False  

{-
  Check if [Result] is a sequence.
-}
isASequence :: [Result] -> Bool
isASequence xs = case xs of
  [] -> True
  [_] -> True
  (x:y:rest) -> case (x, y) of
    (Int n1, Int n2) -> n2 == n1 + 1 && isASequence (y:rest)
    _ -> False


