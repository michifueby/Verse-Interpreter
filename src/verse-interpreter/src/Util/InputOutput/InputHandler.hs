{-
    Project:    Verse-Interpreter
    File:       InputHandler.hs
-}

module Util.InputOutput.InputHandler
    ( getContent
    ) where

import System.IO 

{-
    Get content from the file.
-}
getContent:: String -> IO String
getContent = readFile
