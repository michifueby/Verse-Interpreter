{-
    Project:    Verse-Interpreter
    File:       Lexer.hs
-}

module Util.Lexical.Lexer
     ( NumType, Name, Token(..), lexString, lexer, getTokenName, 
     getTokenNumber, toRowNumber, filterUnusedRows
    ) where

import Data.Char ( isSpace, isDigit )
import Util.Datatypes.IntegerValue ( NumType, toNumType )
import Util.Shared (Name)
import Util.Lexical.LexerAdditionals
import Util.Interface.Token


{-
    Takes a string and returns the corresponding list
    of lexical tokens.  It uses a regular grammar to group characters
    into parenthesis characters, unsigned integers, and names. It
    skips whitespace characters.
-}
lexString :: String -> [Token]
lexString []  = []
lexString xs@(x:xs')
    | x == '\n'  = TokenNewLine : lexString xs'
    | isSpace x  = lexString xs'
    | x == ';'   = TokenSemicolon : lexString xs'
    | x == '('   = TokenLeft Rounded : lexString xs'
    | x == ')'   = TokenRight Rounded : lexString xs'
    | x == ':'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '=' && xs' /= [] && head xs' == '>' = TokenPrimitives "=>" : lexString (drop 1 xs')
    | x == '='   = TokenPrimitives (charToString x) : lexString xs'
    | x == '}'   = TokenRight Curly : lexString xs'
    | x == '{'   = TokenLeft Curly : lexString xs'
    | x == '['   = TokenLeft Squared : lexString xs'
    | x == ']'   = TokenRight Squared : lexString xs'
    | x == '+' && xs' /= [] && head xs' == '+' = TokenPrimitives "++" : lexString (drop 1 xs')   -- =====Add for LP4 Logic Programmin Michael Füby
    | x == '+'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '>'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '<'   = TokenPrimitives (charToString x) : lexString xs'
    | x == ','   = TokenPrimitives (charToString x) : lexString xs'
    | x == '|'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '-'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '*'   = TokenPrimitives (charToString x) : lexString xs'
    | x == '.' && xs' /= [] && head xs' == '.' = TokenPrimitives ".." : lexString (drop 1 xs')
    | x == '.' = error ("Invalid token " ++ "\"" ++ charToString x ++ "\"")
    | x == '"' = processString xs' -- =====Add for LP4 Logic Programmin Michael Füby
    | isDigit x   = let (num,rest) = span isDigit xs
                    in TokenNumber (convertNumType num) : lexString rest
    | otherwise  = let isNameChar c =
                            not (isSpace c || elem c nonNameChars)
                       (name,rest) = span isNameChar xs
                   in  TokenName name : lexString rest
-- =====Add for LP4 Logic Programmin Michael Füby
processString :: String -> [Token]
processString = buildString
-- =====Add for LP4 Logic Programmin Michael Füby
buildString :: [Char] -> [Token]
buildString ('\"' : ts) = lexString ts
buildString (t:ts) = TokenString t : buildString ts
 
{-
    Characters that cannot appear in names: ( ) ; : .....
-}
nonNameChars :: [Char]
nonNameChars = ['(', ')', ';', ':', '{', '}', '=', '[', ']', '+', '-', '*', '<', '>', ',', '.', '|']

{-
    Gets the next list element.
-}
nextElement :: (Eq a) => [a] -> a -> Maybe a
nextElement [] _ = Nothing
nextElement l x = if null f then Just(head l) else Just(head f)
    where f = following x l

{-
    Returns the part of the list after the given element.
-}
following :: (Eq a) => a -> [a] -> [a]
following _ [] = []
following x (y:l) = if x == y then l else following x l

{-
    Converts a char to string.
-}
charToString :: Char -> String
charToString c = [c]

{-
    Converts and unsigned number string into a NumType value.  
    If that cannot be done, it prints an error.
-}
convertNumType :: String -> NumType
convertNumType num  =
    case toNumType num of
        Right v  -> v
        Left err -> error ("Lexical error: " ++ err)

{-  Takes a string and returns the corresponding list of lexical tokens, including the separating keywords and primitive function names
    Example: 
    Input: 1 + 1;\n1 + 2;\n1 + 3;\n      
    Output: [TokenNumber 1,TokenPrimitves "+",TokenNumber 1,TokenNumber 1,TokenPrimitves "+",TokenNumber 2,TokenNumber 1,TokenPrimitves "+",TokenNumber 3]
-}
lexer :: String -> [Token]
lexer xs = markSpecialsTokens (lexString xs)

{- 
    Marks the special tokens
-}
markSpecialsTokens :: [Token] -> [Token]
markSpecialsTokens = map processToken

{- 
    Transform the name tokens to encode keywords and primitive function names in separate categories.
-}
processToken :: Token -> Token
processToken (TokenName n)
   | n `elem` keywords   = TokenKey n
   | n `elem` primitives = TokenPrimitives n
processToken t           = t

{-
    Represents the keywords and primitives in a list.
    Name Type from Datatypes.IntegerValue ( NumType, Name, toNumType )
-}
keywords, primitives :: [Name]           -- =====Add for LP4 Logic Programmin Michael Füby
keywords     = ["if", "then", "else", "int", "false?", "for", "do", "array", "tuple", "data"]
primitives   = ["+","-","*","=","<",">", "|", "..", ",", "?", ":", ".", "=>"]

{-
    Build token list to extended token with row number.
-}
toRowNumber :: [Token] -> [ExtendedToken]
toRowNumber [] = []
toRowNumber tokens = merge 1 tokens []
  where
    merge _ [] [] = []
    merge n [] acc = [(n, reverse acc)]
    merge n (TokenNewLine : ts) acc = (n, reverse acc) : merge (n + 1) ts []
    --merge n (TokenSemicolon : ts) acc = (n, reverse acc) : merge (n + 0) ts []
    merge n (t : ts) acc = merge n ts (t : acc)

{-
    Filter the unused rows from the ExtendedToken list.
-}
filterUnusedRows :: [ExtendedToken] -> [ExtendedToken]
filterUnusedRows = filter (not . null . getTokens)
  
sameTokenNextToEachOther :: [Token] -> Bool
sameTokenNextToEachOther [] = False
sameTokenNextToEachOther [_] = False
sameTokenNextToEachOther (TokenNumber _ : TokenNumber _ : _ ) = True
sameTokenNextToEachOther (TokenPrimitives _ : TokenPrimitives _ : _ ) = True
sameTokenNextToEachOther (_ : rest) = sameTokenNextToEachOther rest

