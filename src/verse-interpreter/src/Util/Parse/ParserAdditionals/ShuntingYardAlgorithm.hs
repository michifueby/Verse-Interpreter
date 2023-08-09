{-
    Project:    Verse-Interpreter
    File:       ShuntingYardAlgorithm.hs
-}

module Util.Parse.ParserAdditionals.ShuntingYardAlgorithm where

import Util.Interface.Token
import Util.Shared
import Util.Interface.AbstractSyntaxTree
import Util.Lexical.Lexer
import Util.Lexical.LexerAdditionals
import Util.Parse.ParserAdditionals

{-
    Transfrom token list to shunting yard list.
-}
toShuntingYard :: [Token] -> [Token]
toShuntingYard tokens = transform (map Just tokens) 

{-
    Returns the precidence of the Token.
-}
precidence :: Token -> Int
precidence token@(TokenPrimitives "+") = 1
precidence token@(TokenPrimitives "-") = 1
precidence token@(TokenPrimitives "*") = 2
precidence token@(TokenPrimitives "<") = 0
precidence token@(TokenPrimitives ">") = 0
precidence token@(TokenPrimitives "=") = 0
precidence token@(TokenLeft Rounded) = 3
precidence token@(TokenRight Rounded) = 3
precidence token@(TokenNumber _) = 4

{-
    Checks if the operator is valid.
-}
isOperator :: Token -> Bool
isOperator (TokenPrimitives "+") = True
isOperator (TokenPrimitives "*") = True
isOperator (TokenPrimitives "-") = True
isOperator (TokenPrimitives ">") = True
isOperator (TokenPrimitives "<") = True
isOperator (TokenPrimitives "=") = True
isOperator _ = False

{-
    Transform the input token list to shunting yard token list.
-}
transform :: [Maybe Token] -> [Token]
transform ts = transform' ts [] [] where
-- No more tokens
    transform' [] [] q = q
    transform' [] s q =
        if head s == TokenLeft Rounded 
            then error "Mismatched Parentheses in operator expression!" 
            else transform' [] (tail s) (q ++ [head s])
    transform' (x:xs) s q = case x of
        Nothing -> error "Illegal tokens in operator expression!"
        (Just (TokenNumber n)) -> transform' xs s (q ++ [TokenNumber n])
        (Just (TokenName n)) -> transform' xs s (q ++ [TokenName n])
        (Just (TokenLeft Rounded)) -> transform' xs (TokenLeft Rounded:s) q
        (Just (TokenRight Rounded)) -> transform' xs s0 q0 where
            s0 = tail $ dropWhile (/= TokenLeft Rounded) s
            q0 = q ++ takeWhile (/= TokenLeft Rounded) s
        (Just o1) -> transform' xs s1 q1 where
            cond o2 = isOperator o2 && (precidence o1 < precidence o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl