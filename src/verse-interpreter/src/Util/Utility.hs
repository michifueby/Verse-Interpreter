{-
    Project:    Verse-Interpreter
    File:       Utility.hs
-}

module Util.Utility
    ( toAddApplicative, isSequence
    ) where

import Data.Char

toAddApplicative :: (Num a) => [[a]] -> [a]
toAddApplicative = foldl (\acc xs -> (+) <$> acc <*> xs) (pure 0)


isSequence :: (Num a, Eq a) => [a] -> Bool
isSequence [] = True
isSequence [_] = True
isSequence (this:other:rest)   
  | other == this + 1 || other == this - 1 = isSequence (other:rest)    
  | otherwise = False  

-- data Token = TokenNumber Int | TokenPrimitives String
-- [TokenNumber 3, TokenNumber 2] -> True
-- [TokenNumber 3, TokenNumber 3] -> True
-- [TokenPrimitive "+", TokenPrimitive "+"] -> True
-- [TokenPrimitive "+", TokenPrimitive "-"] -> True
-- [TokenNumber 3, TokenPrimitive "-", TokenPrimitive "-", TokenNumber 3] -> True
-- [TokenPrimitive "-", TokenNumber 3, TokenNumber 5, TokenPrimitive "-"] -> True
-- [TokenPrimitive "-", TokenPrimitive "-", TokenNumber 3, TokenNumber 3] -> True
-- [TokenPrimitive "+", TokenNumber 2, TokenPrimitive "+"] -> False
-- [TokenNumber 2, TokenPrimitive "+", TokenNumber 2] -> False
-- [TokenNumber 3, TokenPrimitive "+"] -> False
-- [TokenPrimitive "+", TokenNumber 3] -> False

{-sameTokenNextToEachOther [] = False
sameTokenNextToEachOther (x:y:z:xs) = all (\v -> case (x, y, z, v) of
                                                ((TokenNumber _):(TokenNumber _)) -> True
                                                ((TokenPrimitives _):(TokenPrimitives _)) -> True
                                                _ -> False) xs && sameTokenNextToEachOther xs
-}

{-
    sameTokenNextToEachOther :: [Token] -> Bool
    sameTokenNextToEachOther [] = False
    sameTokenNextToEachOther tokens@((TokenNumber _):tokens_) = False
    sameTokenNextToEachOther tokens@((TokenNumber _):(TokenNumber _):tokens_) = True
    sameTokenNextToEachOther tokens@((TokenPrimitives _):(TokenPrimitives _):tokens_) = True
-}