{-
    Project:    Verse-Interpreter
    File:       ParserAdditionals.hs
-}

module Util.Parsing.ParserAdditionals where


import Util.Interface.AbstractSyntaxTree


{- 
    -------------------------------------------
    |            Parser Additionals           |                
    -------------------------------------------
-}

{-
    Define the Parse Error.
-}
type ParseError = String

{-
    Extract Either from Either ParseError VerseExp to VerseExp.
-}
extractEither :: Either ParseError VerseExp -> VerseExp
extractEither (Right r) = r
extractEither (Left l) = error l