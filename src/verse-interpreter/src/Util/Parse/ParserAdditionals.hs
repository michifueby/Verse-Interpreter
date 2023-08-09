{-
    Project:    Verse-Interpreter
    File:       ParserAdditionals.hs
-}

module Util.Parse.ParserAdditionals where 

import Util.Interface.AbstractSyntaxTree
import Util.Lexical.LexerAdditionals
import Util.Interface.Token


{-
    Represents the parse error.
-}
type ParseError = String

{-
    Extract Right from Either ParseError VerseExp to VerseExp.
-}
extractRight :: Either ParseError VerseExp -> VerseExp
extractRight (Right r) = r
extractRight (Left l) = error l

{-
    Extract Right from Either ParseError [Token] to [Token].
-}
toRight :: Either ParseError [Token] -> [Token]
toRight (Right t) = t
toRight (Left s) = error (show s)

{-
    Extract Right from Either ParseError ExtendedToken to ExtendedToken.
-}
toRightExtendedToken :: Either ParseError ExtendedToken -> ExtendedToken
toRightExtendedToken (Right t) = t
toRightExtendedToken (Left s) = error (show s)

{-
    Build error message.

    Example:
    Error in line: 1 | Error message 
-}
printError :: RowNumber -> ParseError -> ParseError
printError l e = "Error on line: " ++ show l ++ " | " ++ e

{-
    Removes a specific element from a list.
-}
removeSpecificElement :: Eq a => a -> [a] -> [a]
removeSpecificElement element = filter (/= element)
{-
    Get body of the expression.
-}
getBetween :: [[Token]] -> [[Token]]
getBetween [] = []
getBetween (_:xs) = init xs

{-
    Remove the outer expression brackets.
-}
removeOuterBrackets :: [[Token]] -> [[Token]]
removeOuterBrackets [] = []
removeOuterBrackets t = [tail(head t)] ++ getBetween t ++ [init(last t)]

{-
    Remove specific brackets from [[Token]].
-}
removeBrackets :: [[Token]] -> [[Token]]
removeBrackets = map processBrackets

{-
    Process the token list to remove specific brackets.
-}
processBrackets :: [Token] -> [Token]
processBrackets tokens
    | TokenLeft Rounded `elem` tokens = removeSpecificElement (TokenLeft Rounded) tokens
    | TokenRight Rounded `elem` tokens = removeSpecificElement (TokenRight Rounded) tokens
processBrackets tokens = tokens
