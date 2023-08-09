
module Util.Interface.Token.ExtendedToken where

import Util.Interface.Token.Token
import Util.Datatypes.IntegerValue
import Util.Shared


{- 
    -------------------------------------------
    |             Lexer Additionals           |                
    -------------------------------------------
-}

{-
    Represents the RowNumber type.
-}
type RowNumber = Int

{-
    Represents the ExtendedToken type.
-}
type ExtendedToken = (RowNumber, [Token])

{-
    Gets the row number from the ExtendedToken.
-}
getRowNumber :: ExtendedToken -> RowNumber
getRowNumber = fst

{-
    Gets the token list from the ExtendedToken.
-}
getTokens :: ExtendedToken -> [Token]
getTokens = snd