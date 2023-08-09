{-
    Project:    Verse-Interpreter
    File:       CommandLineArgumentsParser.hs
-}

module Util.CommandLineArguments.CommandLineArgumentsParser where


{- 
    --------------------------------
    |Command line arguments parser |
    --------------------------------
-}

{-
    Represents the Command type.
-}
data Command = FilePath String 
    | DebugMode Bool
    | WithUserDefinedFunctions Bool
    | WithAllOutput Bool
    | Help 
    deriving (Show, Eq)

{-
    Represents the CommandAction type.
-}
data CommandAction = FullFilePath String 
    | IsDebugModeOn Bool
    | IsWithAllOutput Bool
    | IsUserDefinedFunctionsOn Bool

{-
    Parse the command line args.
-}
parseArgs :: [String] -> [Maybe Command]
parseArgs ("-h":rest) = Just Help : parseArgs rest
parseArgs ("--help":rest) = Just Help : parseArgs rest
parseArgs ("-d":rest) = Just (DebugMode True) : parseArgs rest
parseArgs ("--debug":rest) = Just (DebugMode True) : parseArgs rest
parseArgs ("-a":rest) = Just (WithAllOutput True) : parseArgs rest
parseArgs ("--all":rest) = Just (WithAllOutput True) : parseArgs rest
parseArgs ("-o":rest) = Just (WithAllOutput False) : parseArgs rest
parseArgs ("--one":rest) = Just (WithAllOutput False) : parseArgs rest
parseArgs ("-p":rest) = Just (WithUserDefinedFunctions True) : parseArgs rest
parseArgs ("--path":rest) = Just (WithUserDefinedFunctions True) : parseArgs rest
parseArgs (filePath:rest) = Just (FilePath filePath) : parseArgs rest
parseArgs _ = [Nothing]

{-
    Extract the FilePath from [Maybe Command].
-}
extractFilePath :: [Maybe Command] -> Command
extractFilePath [Just Help,Nothing] = Help
extractFilePath [Nothing] = error (show "Empty file path!")
extractFilePath [] = error (show "Empty file path!")
extractFilePath a@((Just (FilePath f)):rest) = FilePath f

{-
    Extract the DebugMode from [Maybe Command].
-}
extractDebugMode :: [Maybe Command] -> Command
extractDebugMode a@((Just (DebugMode d)):rest) = DebugMode d
extractDebugMode (t:rest) = extractDebugMode rest
extractDebugMode _ = DebugMode False

{-
    Extract the WithUserDefinedFunctions from [Maybe Command].
-}
extractUserDefinedFunctions :: [Maybe Command] -> Command
extractUserDefinedFunctions a@((Just (WithUserDefinedFunctions d)):rest) = WithUserDefinedFunctions d
extractUserDefinedFunctions (t:rest) = extractUserDefinedFunctions rest
extractUserDefinedFunctions _ = WithUserDefinedFunctions False

{-
    Extract the WithAllOutput from [Maybe Command].
-}
extractWithAllOutput :: [Maybe Command] -> Command
extractWithAllOutput a@((Just (WithAllOutput d)):rest) = WithAllOutput d
extractWithAllOutput (t:rest) = extractWithAllOutput rest
extractWithAllOutput _ = WithAllOutput False
  
{-
    Process the command line args.
-}
processCommand :: Command -> Either String CommandAction
processCommand Help = Left "Usage: versify [file path] FilePath [-d] DebugMode [-p] With user defined functions [-a] With All output"
processCommand (DebugMode debugMode) = Right (IsDebugModeOn debugMode)
processCommand (WithUserDefinedFunctions userDefinedFunctions) = Right (IsUserDefinedFunctionsOn userDefinedFunctions)
processCommand (WithAllOutput withAllOutput) = Right (IsWithAllOutput withAllOutput)
processCommand (FilePath filePath) = Right (FullFilePath filePath)

{-
    Extract Right (CommandAction) from Either String CommandAction.
-}
extractRightCommandAction :: Either String CommandAction -> CommandAction
extractRightCommandAction (Right r) = r
extractRightCommandAction (Left l) = error l

{-
    Extract file path from CommandAction.
-}
extractFilePathFromCommandAction :: CommandAction -> String
extractFilePathFromCommandAction (FullFilePath f) = f

{-
    Extract debug mode state from CommandAction.
-}
extractDebugModeStateFromCommandAction :: CommandAction -> Bool
extractDebugModeStateFromCommandAction (IsDebugModeOn d) = d

{-
    Extract all output state from CommandAction.
-}
extractWithAllOutputStateFromCommandAction :: CommandAction -> Bool
extractWithAllOutputStateFromCommandAction (IsWithAllOutput d) = d

{-
    Extract user defined functions state from CommandAction.
-}
extractUserDefinedFunctionsStateFromCommandAction :: CommandAction -> Bool
extractUserDefinedFunctionsStateFromCommandAction (IsUserDefinedFunctionsOn d) = d
