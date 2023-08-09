{-
    Project:    Verse-Interpreter
    File:       Shared.hs
-}

module Util.Shared where 


{- 
    -------------------------------------------
    |                 Shared                  |                
    -------------------------------------------
-}

{-
    Represents the Name type.
-}
type Name = String



data Representation = One | All deriving(Eq, Ord)