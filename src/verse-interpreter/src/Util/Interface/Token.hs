{-
    Project:    Verse-Interpreter
    File:       Token.hs
-}

module Util.Interface.Token where

import Util.Datatypes.IntegerValue (NumType)
import Util.Shared
import Util.Datatypes.StringValue (StringType)


{- 
    Declare Token type.
-}
data Token = TokenLeft BracketType     -- left parenthesis
    | TokenRight BracketType           -- right parenthesis
    | TokenSemicolon                   -- ;
    | TokenNewLine                     -- \n
    | TokenNumber NumType              -- unsigned integer literal
    | TokenString StringType           
    | TokenName Name                   -- names of variables, etc.
    | TokenPrimitives Name             -- names of primitive functions
    | TokenKey Name                    -- keywords
    | TokenOther Name                  -- other characters
    deriving (Show, Eq)

{-
    Represents the bracket type.
-}
data BracketType = Rounded
    | Squared
    | Curly
    deriving (Show, Eq)

{-
    Gets the int number from the token.
-}
getTokenNumber :: Token -> NumType
getTokenNumber (TokenNumber n) = n

{-
    Gets the name from the token.
-}
getTokenName :: Token -> Name
getTokenName (TokenName n) = n
getTokenName (TokenPrimitives n) = n
getTokenName (TokenKey n) = n
getTokenName (TokenOther n) = n