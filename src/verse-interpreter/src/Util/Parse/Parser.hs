{-
    Project:    Verse-Interpreter
    File:       Parser.hs
-}

module Util.Parse.Parser where

import qualified Util.Datatypes.Types as T

import Data.Either
import Data.Maybe

import Util.Interface.AbstractSyntaxTree
import Util.Lexical.Lexer
import Util.Lexical.LexerAdditionals
import Util.Interface.Token
import Util.Parse.ParserAdditionals
import Util.Parse.ParserAdditionals.ShuntingYardAlgorithm


{-
    Parse the [ExtendedToken] into a [VerseExp].
-}
parse :: [ExtendedToken] -> [VerseExp]
parse = map toAst

{-
    Build ExtendedToken to VerseExp.
-}
toAst :: ExtendedToken -> VerseExp
toAst (row, tokens) = extractRight (assembleAst (row, tokens))

{-
    Assemble ast from the ExtendedToken to the VerseExp.
-}
assembleAst :: ExtendedToken -> Either ParseError VerseExp

-- Function Lambda Definition  f:=(x:int=>x+1);
-- assembleAst (row, (TokenName functionName) : (TokenPrimitives ":") : (TokenPrimitives "=") : TokenLeft Rounded : ts) = Right (assembleFunctionLambdaDefinitionExpression (row, TokenName functionName : TokenPrimitives ":" : TokenPrimitives "=" : TokenLeft Rounded : ts))

-- Define type   -- =====Add for LP4 Logic Programmin Michael Füby
assembleAst (row, (TokenKey "data") : (TokenName name) : (TokenPrimitives "=") : (TokenName typeName) : ts) = Right (assembleOwnTypeExpression (row, (TokenName name) : (TokenPrimitives "=") : (TokenName typeName) : ts))
assembleAst (row, (TokenKey "data") : (TokenName name) : ts) = Left (printError row "Invalid type definition!")
assembleAst (row, (TokenKey "data") : ts) = Left (printError row "Invalid type definition!")

-- Initialization
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : (TokenPrimitives "=") : ts) = Right (Initialization var (extractRight (assembleAst(row, ts))))
assembleAst (row, (TokenPrimitives ":") : (TokenPrimitives "=") : ts) = Left (printError row "A initialization of a variable must begin with a variable name!")
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : (TokenPrimitives ":") : ts) = Left (printError row "A initialization of a variable must not contains several " ++ "\"" ++ ":" ++ "\"" ++ " operators" ++ "!")
-- Int Declaration
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : (TokenKey "int") : ts) = Right (assembleDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : ts))
-- Tuple Deklaration
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : (TokenKey "tuple") : ts) = Right (assembleDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "tuple" : ts))
assembleAst (row, (TokenPrimitives ":") : (TokenKey "int") : ts) = Left (printError row "A variable declaration must begin with an variable name!")
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : (TokenKey _) : (TokenLeft Rounded) : ts) = Left (printError row "Invalid data type on tuple declaration!")
assembleAst (row, (TokenName var) : (TokenPrimitives ":") : ts) = Left (printError row ("Variable declaration: Missing datatype on variable " ++ "\"" ++ show var ++ "\"" ++ "!"))
-- Choice
assembleAst (row, TokenLeft Rounded : TokenLeft Rounded : (TokenNumber n) : (TokenPrimitives "|") : ts) = Right (assembleChoice (row, TokenLeft Rounded : TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts))
assembleAst (row, TokenLeft Rounded : (TokenNumber n) : (TokenPrimitives "|") : ts) = Right (assembleChoice (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts))
assembleAst (row, TokenLeft Rounded : (TokenName n) : (TokenPrimitives "|") : ts) = Right (assembleChoice (row, TokenLeft Rounded : TokenName n : TokenPrimitives "|" : ts))
assembleAst (row, (TokenNumber n) : (TokenPrimitives "|") : ts) = Left (printError row "A choice must not begin without left parenthesis!")
assembleAst (row, (TokenNumber from) : (TokenPrimitives "..") : (TokenNumber to) : ts) = Right (assembleChoice (row, TokenNumber from : TokenNumber to : ts))
-- Array long form
assembleAst (row, TokenKey "array" : TokenLeft Curly : ts) = Right (assembleTuple (row, TokenLeft Curly : ts))
assembleAst (row, TokenLeft Curly : (TokenNumber n) : (TokenPrimitives ",") : ts) = Left (printError row ("Keyword " ++ "\"" ++ "array" ++ "\"" ++ " is missing in long form syntax"))
-- Binary-Operation
assembleAst (row, (TokenLeft Rounded) : (TokenNumber n) : (TokenPrimitives op) : ts) = Right (assembleBinaryOperation (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives op : ts))
assembleAst (row, (TokenNumber n) : (TokenPrimitives op) : ts) = Right (assembleBinaryOperation (row, TokenNumber n : TokenPrimitives op : ts))
assembleAst (row, (TokenLeft Rounded) : (TokenName n) : (TokenPrimitives op) : ts) = Right (assembleBinaryOperation (row, TokenLeft Rounded : TokenName n : TokenPrimitives op : ts))
assembleAst (row, (TokenName n) : (TokenPrimitives op) : ts) = Right (assembleBinaryOperation (row, TokenName n : TokenPrimitives op : ts))
-- Tuple 
assembleAst (row, TokenLeft Rounded : (TokenNumber n) : ts) = Right (assembleTuple (row, TokenLeft Rounded : TokenNumber n : ts))
assembleAst (row, TokenLeft Rounded : (TokenName n) : ts) = Right (assembleTuple (row, TokenLeft Rounded : TokenName n : ts))
-- If-Statement
assembleAst (row, (TokenKey "if") : ts) = Right (assembleIfExpression (row, ts))
-- For-Statement
assembleAst (row, (TokenKey "for") : ts) = Right (assembleForExpression (row, ts))
-- Index Access Method
assembleAst (row, TokenName name : TokenLeft Squared : ts) = Right (assembleIndexAccessMethodExpression (row, TokenName name : TokenLeft Squared : ts))
assembleAst (row, TokenName name : TokenRight Squared : ts) = Left (printError row ("Left parenthesis " ++ "\"" ++ "[" ++ "\"" ++ " is missing on index access method!"))
assembleAst (row, TokenLeft Squared : ts) = Left (printError row ("Collection name is missing on index access method before left parenthesis " ++ "\"" ++ "[" ++ "\"" ++ "!"))
-- Function Definition
assembleAst (row, (TokenName functionName) : (TokenLeft Rounded) : (TokenName name) : (TokenPrimitives ":") : (TokenKey key) : ts) = Right (assembleFunctionDefinitionExpression (row, TokenName functionName : TokenLeft Rounded : TokenName name : TokenPrimitives ":" : TokenKey key : ts))
-- Function Call
assembleAst (row, (TokenName functionName) : (TokenLeft Rounded) : ts) = Right (assembleFunctionCallExpression (row, TokenName functionName : TokenLeft Rounded : ts))
-- String  -- =====Add for LP4 Logic Programmin Michael Füby
assembleAst (row, (TokenString n) : ts) = Right (assembleStringExpression (row, TokenString n : ts))
--Variable
assembleAst (row, (TokenKey key) : ts) = Left (printError row "A variable name must not a keyword " ++ show key ++ "!")
assembleAst (row, (TokenName "-") : (TokenNumber value) : ts) = Right (IntLit (-value))     -- Negative numbers
assembleAst (row, (TokenName name) : ts) = Right (Var name)
-- Value
assembleAst (row, (TokenNumber n) : ts) = Right (IntLit n)                                  -- Positive numbers
assembleAst (row, t:ts) = assembleAst (row, ts) 

{- 
    -------------------------------------------
    |           Own Types                     |                
    -------------------------------------------
-}
assembleOwnTypeExpression :: ExtendedToken -> VerseExp
assembleOwnTypeExpression extendedToken = createOwnTypeAst(toRightExtendedToken(checkGrammarOfStringExpression extendedToken))

createOwnTypeAst :: (RowNumber, [Token]) -> VerseExp
createOwnTypeAst (row, TokenName typeName : TokenPrimitives "=" : TokenName name : ts) = OwnType typeName (processTypeFields (row, ts))

processTypeFields :: (RowNumber, [Token]) -> [VerseExp]
processTypeFields (row, TokenLeft Curly : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = assembleDeclaration (row, [TokenName name, TokenPrimitives ":", TokenKey "int"]) : processTypeFields (row, ts)
processTypeFields (row, TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = [assembleDeclaration (row, [TokenName name, TokenPrimitives ":", TokenKey "int"])]
processTypeFields (row, TokenPrimitives "," : ts) = processTypeFields (row, ts)
processTypeFields (row, ts) = processTypeFields (row, ts)

formatOwnType :: [Token] -> [[Token]]
formatOwnType t = assembleExpression t []
    where 
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenPrimitives "," : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc) 

checkGrammarOfOwnTypeExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfOwnTypeExpression args = check (getTokens args) 
    where
        check (t:ts) = check ts
        check t = Right args

{- 
    -------------------------------------------
    |           String Assembler         |                
    -------------------------------------------
-}
assembleStringExpression :: ExtendedToken -> VerseExp
assembleStringExpression extendedToken = createStringAst(toRightExtendedToken(checkGrammarOfStringExpression extendedToken))

{-
    Create from the token list the String VerseExp.
-}
createStringAst :: (RowNumber, [Token]) -> VerseExp
createStringAst (row, TokenPrimitives "++" : ts) = ConcatString (StringLit (assembleString (row, ts))) (StringLit (assembleString (row, ts)))
createStringAst (row, TokenString n : ts) = StringLit (assembleString (row, TokenString n : ts))


{-
    Assemble the String from the token list.
-}
assembleString :: (RowNumber, [Token]) -> [Char]
assembleString (row, TokenString n : ts) = n : assembleString (row, ts)
assembleString (row, p) = []
assembleString (row, []) = []


checkGrammarOfStringExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfStringExpression args = check (getTokens args) 
    where
        check (t:ts) = check ts
        check t = Right args


{- 
    -------------------------------------------
    |           Declaration Assembler         |                
    -------------------------------------------
-}

{-
    Build declaration AST from ExtendedToken.
-}
assembleDeclaration :: ExtendedToken -> VerseExp
assembleDeclaration extendedToken = createDeclarationAst(toRightExtendedToken(checkGrammarOfDeclarationExpression extendedToken))

{-
    Checks the grammar of the binary operation expression.

    (TokenName var) : (TokenPrimitives ":") : (TokenKey "tuple")
-}
checkGrammarOfDeclarationExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfDeclarationExpression args = check (getTokens args) 
    where
        check (TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Rounded : TokenRight Rounded : ts) = Left (printError (getRowNumber args) "Invalid tuple declaration! Tuple declaration include no data type!")
        check (TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Curly : ts) = Left (printError (getRowNumber args) "Invalid left " ++ "\"" ++ "{" ++  "\"" ++ " paranthesis in tuple declaration!")
        check (TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Squared : ts) = Left (printError (getRowNumber args) "Invalid left " ++ "\"" ++ "[" ++  "\"" ++ " paranthesis in tuple declaration!")
        check (t:ts) = check ts
        check t = Right args

{-
    Assembly the Declaration from the token list into a VerseExp.
-}
createDeclarationAst :: (RowNumber, [Token]) -> VerseExp
createDeclarationAst (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : TokenSemicolon : ts) = assembleIntDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : TokenSemicolon : ts)
createDeclarationAst (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : ts) = assembleIntDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : ts)
createDeclarationAst (row, TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Rounded : ts) = assembleTupleDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Rounded : ts)
createDeclarationAst (row, TokenName var : TokenPrimitives ":" : TokenKey _ : TokenLeft Rounded : ts) = error (printError row "Invalid data type in tuple declaration!")

{-
    Assembly the Integer Declaration from the token list into a VerseExp.
-}
assembleIntDeclaration :: (RowNumber, [Token]) -> VerseExp
assembleIntDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : ts) = Declaration var T.Int

{-
    Assembly the Tuple Declaration from the token list into a VerseExp. 
-}
assembleTupleDeclaration :: (RowNumber, [Token]) -> VerseExp
assembleTupleDeclaration (row, TokenName var : TokenPrimitives ":" : TokenKey "tuple" : TokenLeft Rounded : ts) = Declaration var (T.TupleFixed (processTupleTypes (row, ts)))
    where 
          processTupleTypes (row, []) = []
          processTupleTypes (row, TokenKey "int" : TokenPrimitives "," : ts) = T.Int : processTupleTypes (row, ts)
          processTupleTypes (row, TokenKey "int" : TokenRight Rounded : TokenSemicolon : ts) = T.Int : processTupleTypes (row, ts)
          processTupleTypes (row, TokenKey "int" : TokenRight Rounded : ts) = T.Int : processTupleTypes (row, ts)
          processTupleTypes (row, TokenKey "int" : TokenRight p : ts) = error (printError row "Invalid right " ++ "\"" ++ show p ++ "\"" ++ " paranthesis in tuple declaration!")
          processTupleTypes (row, TokenKey "int" : ts) = T.Int : processTupleTypes (row, ts)
          processTupleTypes (row, _ : ts) = error (printError row "Invalid data type in tuple declaration!")


{- 
    -------------------------------------------
    |           Choice Assembler              |
    -------------------------------------------
-}

{-
    Build Choice AST from ExtendedToken.
-}
assembleChoice :: ExtendedToken -> VerseExp
assembleChoice extendedToken = createChoiceAst(toRightExtendedToken(checkGrammarOfChoiceExpression extendedToken))

{-
    Checks the grammar of the Choice expression.
-}
checkGrammarOfChoiceExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfChoiceExpression args = check (getTokens args) 
     where
        check (TokenLeft Rounded : TokenNumber n: TokenPrimitives "|" : TokenPrimitives "|" : ts) = Left (printError (getRowNumber args) ("A choice must not contain several choice operators " ++ "\"" ++ "|" ++ "\"" ++ " after the number " ++ "\"" ++ show n ++ "\"" ++ "!"))
        check (TokenNumber n: TokenPrimitives "|" : TokenPrimitives "|" : ts) = Left (printError (getRowNumber args) ("A choice must not contain several choice operators " ++ "\"" ++ "|" ++ "\"" ++ " after the number " ++ "\"" ++ show n ++ "\"" ++ "!"))
        check (t:ts) = check ts
        check t = Right args

{-
    Assembly the Choice from the token list into a VerseExp.
-}
createChoiceAst :: (RowNumber, [Token]) -> VerseExp
createChoiceAst (row, TokenNumber f: TokenNumber t: r) = Choice (map IntLit [f..t])
createChoiceAst (row, TokenLeft Rounded : TokenNumber n: TokenPrimitives "|" : ts) = Choice (buildChoice(prepareChoice(row, removeBrackets(formatChoice(TokenLeft Rounded : TokenNumber n: TokenPrimitives "|" : ts)))))
createChoiceAst (row, TokenNumber n: TokenPrimitives "|" : ts) = Choice (buildChoice(prepareChoice(row, removeBrackets(formatChoice(TokenNumber n: TokenPrimitives "|" : ts)))))
createChoiceAst (row, TokenLeft Rounded : TokenName n: TokenPrimitives "|" : ts) = Choice (buildChoice(prepareChoice(row, removeBrackets(formatChoice(TokenLeft Rounded : TokenName n: TokenPrimitives "|" : ts)))))
createChoiceAst (row, TokenName n: TokenPrimitives "|" : ts) = Choice (buildChoice(prepareChoice(row, removeBrackets(formatChoice(TokenName n: TokenPrimitives "|" : ts)))))
createChoiceAst (row, TokenSemicolon : ts) = createChoiceAst (row, ts)
createChoiceAst (row, a:as) = createChoiceAst (row, as)

{-
    Build the Choice AST from the [[Token]]. 
-}
buildChoice :: [(RowNumber, [Token])] -> [VerseExp]
buildChoice = map processChoice

{-
    Prepare the choice for the VerseExp.
-}
prepareChoice :: (RowNumber, [[Token]]) -> [(RowNumber, [Token])]
prepareChoice (r, t) = map (\x -> (r, x)) t

{-
    Process the Choice the return the VerseExp.
-}
processChoice :: (RowNumber, [Token]) -> VerseExp
processChoice (row, TokenName n : ts) = Var n
processChoice (row, TokenNumber n : TokenPrimitives p : ts) = createBinOpAstWithUnification (row, TokenNumber n : TokenPrimitives p : ts)
processChoice (row, TokenNumber n : ts) = IntLit n
processChoice (row, TokenRight Rounded : ts) = processChoice (row, ts)
processChoice (row, ts) = processChoice (row, ts)

{-
    Format the Choice expression into [[Token]].
-}
formatChoice :: [Token] -> [[Token]]
formatChoice t = assembleExpression t []
    where 
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenPrimitives "|" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc) 


{- 
    -------------------------------------------
    |           Tuple Assembler               |
    -------------------------------------------
-}

{-
    Build Tuple AST from ExtendedToken.
-}
assembleTuple :: ExtendedToken -> VerseExp
assembleTuple extendedToken = createTupleAst(toRightExtendedToken(checkGrammarOfTupleExpression extendedToken))

{-
    Checks the grammar of the Tuple expression.
-}
checkGrammarOfTupleExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfTupleExpression args = check (getTokens args) 
    where -- Check grammar of Tuple
        check (TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : TokenPrimitives "," : ts) = Left (printError (getRowNumber args) ("The tuple must not contain several " ++ "\"" ++ "," ++ "\"" ++ "after a " ++ "\"" ++ "," ++ "\"" ++ "!"))
        check (TokenNumber n : TokenPrimitives "," : TokenPrimitives "," : ts) = Left (printError (getRowNumber args) ("The tuple must not contain several " ++ "\"" ++ "," ++ "\"" ++ "after a " ++ "\"" ++ "," ++ "\"" ++ "!"))
        check (t:ts) = check ts
        check t = Right args

{-
    Assembly the Tuple from the token list into a VerseExp.
-}
createTupleAst :: (RowNumber, [Token]) -> VerseExp
createTupleAst (row, TokenNumber f : TokenNumber t : r) = Tuple (map IntLit [f..t])
createTupleAst (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts)))))
createTupleAst (row, TokenLeft Rounded : TokenName name : TokenPrimitives "," : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple( TokenLeft Rounded : TokenName name : TokenPrimitives "," : ts)))))
createTupleAst (row, TokenLeft Rounded : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Rounded : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts)))))
createTupleAst (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts)))))
createTupleAst (row, TokenLeft Rounded : TokenNumber n : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Rounded : TokenNumber n : ts)))))
createTupleAst (row, TokenLeft Curly : TokenNumber n : TokenPrimitives "," : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Curly : TokenNumber n : TokenPrimitives "," : ts)))))
createTupleAst (row, TokenLeft Curly : TokenNumber n : ts) = Tuple (buildTuple(prepareTuple(row, removeBrackets(formatTuple(TokenLeft Curly : TokenNumber n : ts)))))
createTupleAst (row, a:as) = createTupleAst (row, as)

{-
    Build the tuple to VerseExp.
-}
--buildTuple :: [[Token]] -> VerseExp
--buildTuple t = Tuple (foldr (\tupleExp acc -> processTuple tupleExp : acc) [] t)
--buildTuple t = Tuple (processTuple (concat t))
--buildTuple tokens = Tuple (map processTuple tokens)

buildTuple :: [(RowNumber, [Token])] -> [VerseExp]
buildTuple = map processTuple

prepareTuple :: (RowNumber, [[Token]]) -> [(RowNumber, [Token])]
prepareTuple (r, t) = map (\x -> (r, x)) t

-- [[TokenNumber 1],[TokenNumber 2],[TokenLeft Rounded,TokenNumber 3],[TokenNumber 4,TokenRight Rounded]]
processTuple :: (RowNumber, [Token]) -> VerseExp
processTuple (row, TokenNumber n : ts) = IntLit n
processTuple (row, TokenName n : TokenPrimitives ":" : TokenKey "int" : ts) = assembleDeclaration (row, TokenName n : TokenPrimitives ":" : TokenKey "int" : ts)
processTuple (row, TokenName n : ts) = Var n
{-processTuple row (TokenLeft Rounded : ts) = Tuple (processNestedTuple ts)
    where 
        processNestedTuple [] = []
        processNestedTuple (TokenNumber n : TokenRight Rounded : ts) = [processTuple (TokenNumber n : ts)]
        processNestedTuple ((TokenNumber n) : ts) = IntLit n : processNestedTuple ts
        processNestedTuple ts = createBinOpAst ts : processNestedTuple ts -}
processTuple (row, t:ts) = processTuple (row, ts)


{-

-- [[TokenNumber 1],[TokenNumber 2],[TokenLeft Rounded,TokenNumber 3],[TokenNumber 4,TokenRight Rounded]]
processTuple :: [Token] -> [VerseExp]
processTuple (TokenNumber n : ts) = IntLit n : processTuple ts
processTuple (TokenLeft Rounded : ts) = [Tuple (processNestedTuple ts [])]
processTuple (t:ts) = processTuple ts

processNestedTuple :: [Token] -> [VerseExp] -> [VerseExp]
processNestedTuple [] [] = []
processNestedTuple [] acc = reverse acc
processNestedTuple (TokenRight Rounded : ts) acc = (reverse acc) : (processTuple ts)
processNestedTuple (TokenLeft Rounded : ts) acc = processNestedTuple ts [] : processNestedTuple ts acc
processNestedTuple ((TokenNumber n) : ts) acc = processNestedTuple ts (IntLit n : acc)
processNestedTuple (t : ts) acc = processNestedTuple ts (processTuple [t] : acc)

-}

{-
    Format the tuple expression.
-}
formatTuple :: [Token] -> [[Token]]
formatTuple t = assembleExpression t []
    where 
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenPrimitives "," : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc) 


{- 
    -------------------------------------------
    |           If-Statement Assembler        |
    -------------------------------------------
-}

{-
    Assemble the If expression to the VerseExp.

    --> if (1 < 2 | 3 > 4, 2 + 3) then 3 else 2
-}
assembleIfExpression :: ExtendedToken -> VerseExp
assembleIfExpression extendedToken = createIfAst(formatIf(toRight(checkGrammarOfIfExpression extendedToken)))

{-
    Checks the grammar of the if expression.
-}
checkGrammarOfIfExpression :: ExtendedToken -> Either ParseError [Token]
checkGrammarOfIfExpression args = check (getTokens args) 
     where -- Check complete grammar of if!!!!!!!
        check (t:ts) = check ts
        check t = Right (getTokens args)

{-
    Build the VerseExp from the [[Token]].
-}
createIfAst :: [[Token]] -> VerseExp
createIfAst (c:t:e:r) = If (assembleCondition c) (createBinOpAst t) (createBinOpAst e)
createIfAst _ = error "Caused In If" 

{-
    Assemble the condition from the token list.
-}
assembleCondition :: [Token] -> VerseExp
assembleCondition c 
    | TokenPrimitives "|" `elem` c && head c == TokenLeft Rounded && last c == TokenRight Rounded = buildCondition(prepareCondition(removeOuterBrackets(splitCondition c)))
    | TokenPrimitives "," `elem` c && head c == TokenLeft Rounded && last c == TokenRight Rounded = buildCondition(prepareCondition(removeOuterBrackets(splitCondition c)))
    | head c /= TokenLeft Rounded = error "If expression: Left bracket is missing in condition expression!"
    | last c /= TokenRight Rounded = error "If expression: Right bracket is missing in condition expression!"
    | otherwise = createBinOpAst c

{-
    Split the condition from the If expression.
-}
splitCondition :: [Token] -> [[Token]]
splitCondition c = assembleCondition c []
    where 
        assembleCondition [] [] = []
        assembleCondition [] acc = [reverse acc]
        assembleCondition (TokenPrimitives "|" : ts) acc = reverse acc : [TokenPrimitives "|"] : assembleCondition ts [] 
        assembleCondition (TokenPrimitives "," : ts) acc = reverse acc : [TokenPrimitives ","] : assembleCondition ts [] 
        assembleCondition (t : ts) acc = assembleCondition ts (t : acc)

{-
    Shift the condition from the [[Token]] and build a new [[Token]].
-}
prepareCondition :: [[Token]] -> [[Token]]
prepareCondition [] = []
prepareCondition [x] = [x]
prepareCondition (left:right:rest)
    | left == [TokenPrimitives ","] = right:left:prepareCondition rest
    | left == [TokenPrimitives "|"] = right:left:prepareCondition rest
    | otherwise = left:prepareCondition (right:rest)

{-
    Build the condition part from the If expression.
-}
buildCondition :: [[Token]] -> VerseExp
buildCondition tokens = assemble tokens []
    where
        assemble [] [] = error "buildCondition: Empty"
        assemble [] [t] = t
        assemble (t:ts) stack = 
            case (t, stack) of 
                ([TokenPrimitives ","], right:left:stack) -> assemble ts (And left right : stack)
                ([TokenPrimitives "|"], right:left:stack) -> assemble ts (Or left right : stack)
                _ -> assemble ts (createBinOpAst t : stack)
        
{-
    Format the If expression from the token list into a token sub list.
-}
formatIf :: [Token] -> [[Token]]
formatIf c = assembleExpression c []
    where
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenKey "then" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (TokenKey "else" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)


{- 
    -------------------------------------------
    |           For Assembler                 |
    -------------------------------------------
-}

{-
    Build For AST from ExtendedToken.

    BasicFor Domain 
             VerseExp

    ForDo Domain       Range
          VerseExp     VerseExp
-}
assembleForExpression :: ExtendedToken -> VerseExp
assembleForExpression extendedToken = createForAst(toRightExtendedToken(checkGrammarOfForExpression extendedToken))

{-
    Checks the grammar of the Choice expression.
-}
checkGrammarOfForExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfForExpression args = check (getTokens args) 
     where -- Check complete grammar of for!!!!!!!
        check (t:ts) = check ts
        check t = Right args

{-
    Assembly the For expression from [Token] into a VerseExp.
-}
createForAst :: (RowNumber, [Token]) -> VerseExp
createForAst (row, TokenLeft Curly : TokenNumber firstNumber : TokenPrimitives ".." : TokenNumber secondNumber : r) = BasicFor (Choice (map IntLit [firstNumber..secondNumber]))
createForAst (row, TokenLeft Curly : TokenNumber number : TokenPrimitives "|" : ts) = buildFor(removeForBrackets(getInnerFor(row, TokenLeft Curly : TokenNumber number : TokenPrimitives "|" : ts)))
createForAst (row, TokenLeft Curly : TokenNumber number : ts) = BasicFor (IntLit number)
createForAst (row, TokenLeft Curly : TokenKey "false?" : ts) = BasicFor Fail
--createForAst (row, TokenLeft Curly : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = buildForAdditionals(getInnerForAdditionals(row, TokenName name : TokenPrimitives ":" : TokenKey "int" : ts))
createForAst (row, TokenLeft Curly : TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = buildForDo(getInnerForDo(row, TokenName name : TokenPrimitives ":" : TokenKey "int" : ts))
createForAst (row, TokenLeft Rounded : TokenName name : ts) = buildForDo(getInnerForDo(row,TokenLeft Rounded : TokenName name : ts))
createForAst (row, a:as) = createForAst (row, as)

{-
    Build the BasicFor VerseExp.      for{i:int; as[i]+1} -> create new VerseExp with [Domain]
-}
buildFor :: [[Token]] -> VerseExp
buildFor tokens = BasicFor (Choice (map processFor tokens))

buildForAdditionals :: (RowNumber, [[Token]]) -> VerseExp
buildForAdditionals (row, domain : ts) = BasicForAdditionals (processForAdditionals (row, domain))

processForAdditionals :: (RowNumber, [Token]) -> [VerseExp]
processForAdditionals (row, []) = []
processForAdditionals (row, TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = assembleDeclaration (row, TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) : processForAdditionals (row, ts)
processForAdditionals (row, TokenName name : TokenPrimitives ":" : TokenKey "tuple" : ts) = assembleDeclaration (row, TokenName name : TokenPrimitives ":" : TokenKey "tuple" : ts) : processForAdditionals (row, ts)
processForAdditionals (row, TokenName name : TokenLeft Squared : ts) = assembleIndexAccessMethodExpression (row, TokenName name : TokenLeft Squared : ts) : processForAdditionals (row, ts)
processForAdditionals (row, TokenSemicolon : ts) = processForAdditionals (row, ts)
processForAdditionals (row, t:ts) = processForAdditionals (row, ts)

{-
    Build the ForDo VerseExp.
-}
buildForDo :: (RowNumber, [[Token]]) -> VerseExp
buildForDo (row, domain : range : ts) = ForDo (processForDoDomain (row, domain)) (processForDoRange (row, range))

{-
    Process the For expression.
-}
processFor :: [Token] -> VerseExp
processFor (TokenNumber n : ts) = IntLit n 
processFor (t:ts) = processFor ts

{-
    Process the ForDo Domain expression.
-}
processForDoDomain :: (RowNumber, [Token]) -> VerseExp
processForDoDomain (row, tokens) = processDomain (row, tokens)
    where processDomain (row, TokenName name : TokenPrimitives ":" : TokenPrimitives "=" : ts) = Initialization name (processDomain(row, ts))
          processDomain (row, TokenName var : TokenPrimitives ":" : TokenKey "int" : ts) = Declaration var T.Int
          processDomain (row, TokenNumber from : TokenPrimitives ".." : TokenNumber to : ts) = assembleChoice (row, TokenNumber from : TokenNumber to : ts)
          processDomain (row, TokenNumber n : TokenPrimitives op : ts) = assembleBinaryOperation (row, removeForDoBrackets(TokenNumber n : TokenPrimitives op : ts))
          processDomain (row, TokenName n : TokenPrimitives op : ts) = assembleBinaryOperation (row, removeForDoBrackets(TokenName n : TokenPrimitives op : ts))
          --processDomain (row, TokenPrimitives "," : ts) = Add AND
          processDomain (row, t:ts) = processDomain (row, ts)

{-
    Process the ForDo Range expression.
-}
processForDoRange :: (RowNumber, [Token]) -> VerseExp
processForDoRange (row, tokens) = processRange (row, tokens)
    where 
          processRange (row, TokenName n : TokenLeft Squared : ts) = assembleIndexAccessMethodExpression(row, removeForDoBrackets(TokenName n : TokenLeft Squared : removeSpecificElement (TokenRight Curly) ts))
          processRange (row, TokenName n : TokenPrimitives "|" : ts) = assembleChoice (row, TokenName n : TokenPrimitives "|" : ts)
          processRange (row, TokenNumber n : TokenPrimitives "|" : ts) = assembleChoice (row, TokenNumber n : TokenPrimitives "|" : ts)
          processRange (row, TokenLeft Rounded : TokenName name : TokenPrimitives "|" : ts) = assembleChoice(row, removeForDoBrackets(TokenLeft Rounded : TokenName name : TokenPrimitives "|" : ts))
          processRange (row, TokenName name : TokenPrimitives p : ts) = assembleBinaryOperation (row, removeForDoBrackets(TokenName name : TokenPrimitives p : ts))
          processRange (row, TokenLeft Rounded : TokenName name : TokenPrimitives p : ts) = assembleBinaryOperation(row, removeForDoBrackets(TokenLeft Rounded : TokenName name : TokenPrimitives p : ts))
          processRange (row, t:ts) = processRange (row, ts)

{-
    Gets the body of the BasicFor expression.
-}
getInnerFor :: (RowNumber, [Token]) -> (RowNumber, [[Token]])
getInnerFor (row, tokens) = (row, assembleExpression tokens [])
    where
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenPrimitives "|" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)

getInnerForAdditionals :: (RowNumber, [Token]) -> (RowNumber, [[Token]])
getInnerForAdditionals (row, tokens) = (row, assembleExpression tokens [])
    where
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)

{-
    Gets the body of the ForDo expression.
-}
getInnerForDo :: (RowNumber, [Token]) -> (RowNumber, [[Token]])
getInnerForDo (row, tokens) = (row, assembleExpression tokens [])
    where
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenSemicolon : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (TokenKey "do" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)

{-
    Remove brackets from the for expression.
-}
removeForBrackets :: (RowNumber, [[Token]]) -> [[Token]]
removeForBrackets (row, []) = []
removeForBrackets (row, t) = [tail(head t)] ++ getBetween t ++ [init(last t)]

{-
    Remove the ForDo brackets.
-}
removeForDoBrackets :: [Token] -> [Token]
removeForDoBrackets [] = []
removeForDoBrackets t = [head t] ++ getBetweenForDo t ++ [last t]

{-
    Gets the body of ForDo.
-}
getBetweenForDo :: [Token] -> [Token]
getBetweenForDo [] = []
getBetweenForDo (_:xs) = init xs


{- 
    -------------------------------------------
    |           Binary Operation Assembler    |
    -------------------------------------------
-}

{-
    Build binary operation AST from ExtendedToken.
-}
assembleBinaryOperation :: ExtendedToken -> VerseExp
assembleBinaryOperation extendedToken = createBinOpAstWithUnification(toRightExtendedToken(checkGrammarOfBinaryOperationExpression extendedToken))

{-
    Checks the grammar of the binary operation expression.
-}
checkGrammarOfBinaryOperationExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfBinaryOperationExpression args = check (getTokens args) 
    where
        check (TokenNumber n1: TokenNumber n2: ts) = Left (printError (getRowNumber args) ("Numbers " ++ "\"" ++ show n1 ++ ", " ++ show n2  ++ "\"" ++ " must not sit next to each other!"))
        check (TokenPrimitives op1: TokenPrimitives op2: ts) = Left (printError (getRowNumber args) ("There cannot be two operators " ++ "\"" ++ show op1 ++ ", " ++ show op2 ++ "\"" ++ " subsequent!"))
        check (TokenRight Rounded: TokenNumber n: ts) = Left (printError (getRowNumber args) ("After a parenthesis " ++ "\"" ++ ")" ++ "and number " ++ "\"" ++ show n ++ "\"" ++ "...."))
        check (TokenLeft Rounded: TokenPrimitives op: ts) = Left (printError (getRowNumber args) ("An operator " ++ "\"" ++ show op ++ "\"" ++ "must not come immediately after a parenthesis " ++ "\"" ++ "(" ++ "\""))
        --check (TokenNumber n: TokenPrimitives op: ts) = Left (printError (getRowNumber args) ("After the number " ++ "\"" ++ show n ++ "\"" ++  " and the operator " ++ "\"" ++ show op ++ "\"" ++ " must follow a number!"))
        check (t:ts) = check ts
        check t = Right args

{-
    Assembly the binary operation with unification from the token list into a VerseExp.

    x = (2,y:int);
    x = (z:int,3);
-}
createBinOpAstWithUnification :: (RowNumber, [Token]) -> VerseExp
createBinOpAstWithUnification (row, TokenNumber n : TokenPrimitives "=" : ts) = Unify (IntLit n) (processUnification (row, ts))
createBinOpAstWithUnification (row, TokenName n : TokenPrimitives "=" : ts) = Unify (Var n) (processUnification (row, ts))        
createBinOpAstWithUnification (row, token) = parseVerseExp (toShuntingYard token) []
    where
        parseVerseExp [] [] = error "empty Token List"
        parseVerseExp [] [t] = t
        parseVerseExp ((TokenNumber n):ts) stack = parseVerseExp ts (IntLit n : stack)
        parseVerseExp ((TokenName n):ts) stack = parseVerseExp ts (Var n : stack)
        parseVerseExp ((TokenPrimitives "+"):ts) (right : left : stack) = parseVerseExp ts (Add left right : stack)
        parseVerseExp ((TokenPrimitives "-"):ts) (right : left : stack) = parseVerseExp ts (Sub left right : stack)
        parseVerseExp ((TokenPrimitives "*"):ts) (right : left : stack) = parseVerseExp ts (Mul left right : stack)
        parseVerseExp ((TokenPrimitives "<"):ts) (right : left : stack) = parseVerseExp ts (Lt  left right : stack)
        parseVerseExp ((TokenPrimitives ">"):ts) (right : left : stack) = parseVerseExp ts (Gt  left right : stack)
        --parseVerseExp ((TokenPrimitives "="):ts) (right : left : stack) = parseVerseExp ts (Unify left right : stack)
        parseVerseExp _ _ = error "ERROR int parseVerseExp!"


processUnification :: (RowNumber, [Token]) -> VerseExp
processUnification (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts) = assembleTuple (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts)
processUnification (row, TokenLeft Rounded : TokenName n : TokenPrimitives ":" : TokenKey "int" : ts) = assembleTuple (row, TokenLeft Rounded : TokenName n : TokenPrimitives ":" : TokenKey "int" : ts)
processUnification (row, TokenSemicolon : ts) = processUnification (row, ts)
processUnification (row, TokenNumber n : ts) = IntLit n
processUnification (row, ts) = processUnification (row, ts)

{-
    Assembly the binary operation from the token list into a VerseExp.
-}
createBinOpAst :: [Token] -> VerseExp
createBinOpAst token = parseVerseExp (toShuntingYard token) []
    where
        parseVerseExp [] [] = error "empty Token List"
        parseVerseExp [] [t] = t
        parseVerseExp ((TokenNumber n):ts) stack = parseVerseExp ts (IntLit n : stack)
        parseVerseExp ((TokenName n):ts) stack = parseVerseExp ts (Var n : stack)
        parseVerseExp ((TokenPrimitives "+"):ts) (right : left : stack) = parseVerseExp ts (Add left right : stack)
        parseVerseExp ((TokenPrimitives "-"):ts) (right : left : stack) = parseVerseExp ts (Sub left right : stack)
        parseVerseExp ((TokenPrimitives "*"):ts) (right : left : stack) = parseVerseExp ts (Mul left right : stack)
        parseVerseExp ((TokenPrimitives "<"):ts) (right : left : stack) = parseVerseExp ts (Lt  left right : stack)
        parseVerseExp ((TokenPrimitives ">"):ts) (right : left : stack) = parseVerseExp ts (Gt  left right : stack)
        parseVerseExp ((TokenPrimitives "="):ts) (right : left : stack) = parseVerseExp ts (Eq  left right : stack)
        parseVerseExp _ _ = error "ERROR int parseVerseExp!"


{- 
    -------------------------------------------
    |       Index Access Method Assembler     |
    -------------------------------------------
-}

{-
    Assemble the Index Access Method expression to the VerseExp.
-}
assembleIndexAccessMethodExpression :: ExtendedToken -> VerseExp
assembleIndexAccessMethodExpression extendedToken = createIndexAccessMethodAst(formatIam(toRightExtendedToken(checkGrammarOfIndexAccessMethodExpression extendedToken)))

{-
    Checks the grammar of the Index Access Method expression.
-}
checkGrammarOfIndexAccessMethodExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfIndexAccessMethodExpression args = check (getTokens args) 
    where
        check (TokenName name : TokenLeft Squared : TokenLeft Squared : ts) = Left (printError (getRowNumber args) ("Several left parenthesis " ++ "\"" ++ "[" ++ "\"" ++ " not allowed on index access method!"))
        check (TokenName name : TokenNumber index : TokenRight Squared : ts) = Left (printError (getRowNumber args) ("Left parenthesis " ++ "\"" ++ "[" ++ "\"" ++ " is missing on index access method!"))
        check (TokenLeft Squared : TokenRight Squared : ts) = Left (printError (getRowNumber args) ("Index number is missing on index access method after left parenthesis " ++ "\"" ++ "[" ++ "\"" ++ "!"))
        check (t:ts) = check ts 
        check t = Right args

{-
    Assembly the Function Call from the token list into a VerseExp.
-}
createIndexAccessMethodAst :: (RowNumber, [[Token]]) -> VerseExp
createIndexAccessMethodAst (row, name : index : rest : ts) = processRest row name index rest
    where -- Supports operators after the IAM
        processRest row name index rest@(TokenPrimitives "+" : TokenNumber n : t) = Add (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenNumber n : t))
        processRest row name index rest@(TokenPrimitives "-" : TokenNumber n : t) = Sub (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenNumber n : t))
        processRest row name index rest@(TokenPrimitives "*" : TokenNumber n : t) = Mul (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenNumber n : t))
        processRest row name index rest@(TokenPrimitives "+" : TokenName n : t) = Add (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenName n : t))
        processRest row name index rest@(TokenPrimitives "-" : TokenName n : t) = Sub (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenName n : t))
        processRest row name index rest@(TokenPrimitives "*" : TokenName n : t) = Mul (IAM (buildIam (row, name)) (buildIam (row, index))) (createBinOpAst (TokenName n : t))
createIndexAccessMethodAst (row, name : index : rest) = IAM (buildIam (row, name)) (buildIam (row, index))
createIndexAccessMethodAst _ = error "Error while build IAM!"

{-
    Return the VerseExp for the Index Access Method.
-}
buildIam :: (RowNumber, [Token]) -> VerseExp
buildIam (row, TokenName name : ts) = Var name
buildIam (row, TokenLeft Rounded : TokenLeft Rounded : (TokenNumber n) : (TokenPrimitives "|") : ts) = assembleChoice (row, TokenLeft Rounded : TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts)
buildIam (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts) = assembleChoice (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts)
buildIam (row, TokenLeft Rounded : TokenName n : TokenPrimitives "|" : ts) = assembleChoice (row, TokenLeft Rounded : TokenName n : TokenPrimitives "|" : ts)
buildIam (row, TokenNumber from : TokenPrimitives ".." : TokenNumber to : ts) = assembleChoice (row, TokenNumber from : TokenNumber to : ts)
buildIam (row, TokenNumber n : ts) = assembleBinaryOperation (row, TokenNumber n : ts)

{-
    Format the Index Access Method.
-}
formatIam :: (RowNumber, [Token]) -> (RowNumber, [[Token]])
formatIam (row, c) = (row, assembleExpression c [])
    where
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenLeft Squared : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (TokenRight Squared : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)


{- 
    -------------------------------------------
    |       Function Definition Assembler     |
    -------------------------------------------
-}

{-
    Assemble the Function Definition expression to the VerseExp.

    FuncDef FuncName Parameters FuncBody Return
    type ParamName = Name
    type FuncName = Name
    type Parameter = (ParamName, Type)
    type Parameters = [Parameter]
    type FuncBody = VerseExp
    type Return = Type

    f(x:int ):int := x+1;

    f(x:int ):int := 
    {
        x+1;
    }

    -> Seq [VerseExp] []
-}
assembleFunctionDefinitionExpression :: ExtendedToken -> VerseExp
assembleFunctionDefinitionExpression extendedToken = createFunctionDefinitionAst(formatFunctionDefinition(toRightExtendedToken(checkGrammarOfFunctionDefinitionExpression extendedToken)))

{-
    Checks the grammar of the Function Definition expression.
-}
checkGrammarOfFunctionDefinitionExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfFunctionDefinitionExpression = check
    where
        --check (rowNumber, (t:ts)) = check (rowNumber, ts)
        check (row, tokens) = Right (row, tokens)

{-
    Assembly the Function Definition from the token list into a VerseExp.
-}
createFunctionDefinitionAst :: (RowNumber, [[Token]]) -> VerseExp
createFunctionDefinitionAst ts@(row, funcName : parameters : returnType : body: rest) = FuncDef (processFunctionName funcName) (assembleFunctionDefinitionParameters(row, parameters)) (assembleFunctionDefinitionBody (row, body)) (assembleFunctionDefinitionReturnType (row, returnType))
    where processFunctionName (TokenName name : ts) = name 
          processFunctionName [] = []
createFunctionDefinitionAst _ = error "Error while creating the function definition ast!"

assembleFunctionDefinitionParameters :: (RowNumber, [Token]) -> Parameters
assembleFunctionDefinitionParameters (row, tokens) = processParameters tokens
    where processParameters [] = []
          processParameters (TokenName name : TokenPrimitives ":" : TokenKey "int" : ts) = (name, T.Int) : processParameters ts
          processParameters (TokenPrimitives "," : ts) = processParameters ts
          processParameters (TokenName name : TokenPrimitives ":" : TokenKey _ : ts) = error "Invalid data type!"
          processParameters _ = error "Invalid parameter list!"

assembleFunctionDefinitionReturnType :: (RowNumber, [Token]) -> T.Type
assembleFunctionDefinitionReturnType (row, TokenKey "int" : ts) = T.Int
assembleFunctionDefinitionReturnType (row, TokenKey _ : ts) = error "Invalid return type!"

assembleFunctionDefinitionBody :: (RowNumber, [Token]) -> FuncBody
-- Choices
assembleFunctionDefinitionBody (row, TokenLeft Rounded : (TokenNumber n) : (TokenPrimitives "|") : ts) = assembleChoice (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts)
assembleFunctionDefinitionBody (row, (TokenNumber from) : (TokenPrimitives "..") : (TokenNumber to) : ts) = assembleChoice (row, TokenNumber from : TokenNumber to : ts)
-- Array long form syntax
assembleFunctionDefinitionBody (row, TokenKey "array" : TokenLeft Curly : ts) = assembleTuple (row, TokenLeft Curly : ts)
-- Tuples
--processFunctionCall (row, TokenLeft Rounded : (TokenNumber n) : ts) = assembleTuple (row, TokenLeft Rounded : TokenNumber n : ts)
-- If 
assembleFunctionDefinitionBody (row, (TokenKey "if") : ts) = assembleIfExpression (row, ts)
-- Index Access Method
assembleFunctionDefinitionBody (row, TokenName name : TokenLeft Squared : ts) = assembleIndexAccessMethodExpression (row, TokenName name : TokenLeft Squared : ts)
-- binary operation
assembleFunctionDefinitionBody (row, (TokenNumber n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenNumber n : TokenPrimitives op : ts)
assembleFunctionDefinitionBody (row, (TokenName n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenName n : TokenPrimitives op : ts)
assembleFunctionDefinitionBody (row, (TokenLeft Rounded) : (TokenNumber n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives op : ts)
assembleFunctionDefinitionBody (row, (TokenLeft Rounded) : (TokenName n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenLeft Rounded : TokenName n : TokenPrimitives op : ts)
-- Int value
assembleFunctionDefinitionBody (row, TokenNumber n : ts) = assembleBinaryOperation (row, TokenNumber n : ts)
-- variable
assembleFunctionDefinitionBody (row, TokenName name : ts) = Var name
assembleFunctionDefinitionBody (row, t:ts) = assembleFunctionDefinitionBody (row, ts)

{-
    Format the Function Definition expression.

    example:
    f(x:int ):int := x+1;

    --> [[TokenName "f"],[TokenName "x",TokenPrimitives ":",TokenKey "int"],[TokenKey "int"],[TokenName "x",TokenPrimitives "+",TokenNumber 1]]
-}
formatFunctionDefinition :: (RowNumber, [Token]) -> (RowNumber, [[Token]])
formatFunctionDefinition (r, t) = (r, assembleExpression t [])
    where 
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenLeft Rounded : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (TokenRight Rounded : TokenPrimitives ":" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (TokenPrimitives ":" : TokenPrimitives "=" : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)


{- 
    -------------------------------------------
    |        Function Lambda Assembler        |
    -------------------------------------------
-}

{-
    Assemble the Function Lambda expression to the VerseExp.

    FuncDef FuncName Parameters FuncBody Return
    type ParamName = Name
    type FuncName = Name
    type Parameter = (ParamName, Type)
    type Parameters = [Parameter]
    type FuncBody = VerseExp
    type Return = Type

    example: f:=(x:int=>x+1);
-}
--assembleFunctionLambdaDefinitionExpression :: ExtendedToken -> VerseExp
--assembleFunctionLambdaDefinitionExpression extendedToken = createFunctionLambdaDefinitionAst(formatFunctionDefinition(toRightExtendedToken(checkGrammarOfFunctionDefinitionExpression extendedToken)))

{-
    Checks the grammar of the Function Definition expression.
-}
checkGrammarOfFunctionLambdaExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfFunctionLambdaExpression = check
    where
        --check (rowNumber, (t:ts)) = check (rowNumber, ts)
        check (row, tokens) = Right (row, tokens)

{-
    Assembly the Function Definition from the token list into a VerseExp.
-}
--createFunctionLambdaDefinitionAst :: (RowNumber, [[Token]]) -> VerseExp


{- 
    -------------------------------------------
    |       Function Call Assembler           |
    -------------------------------------------
-}

{-
    Assemble the Function Call expression to the VerseExp.
    
    AST: FuncCall FuncName FuncArgs 
-}
assembleFunctionCallExpression :: ExtendedToken -> VerseExp
assembleFunctionCallExpression extendedToken = createFunctionCallAst(toRightExtendedToken(checkGrammarOfFunctionCallExpression extendedToken))

{-
    Checks the grammar of the Function Call expression.
-}
checkGrammarOfFunctionCallExpression :: ExtendedToken -> Either ParseError ExtendedToken
checkGrammarOfFunctionCallExpression = check
    where
        --check (rowNumber, (t:ts)) = check (rowNumber, ts)
        check (row, tokens) = Right (row, tokens)

{-
    Assembly the Function Call from the token list into a VerseExp.
-}
createFunctionCallAst :: ExtendedToken -> VerseExp
createFunctionCallAst (row, TokenName functionName : TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts) = FuncCall functionName (buildFunctionCall (prepareFunctionCall(row, removeOuterBrackets(formatFunctionCall(TokenLeft Rounded : TokenNumber n : TokenPrimitives "," : ts)))))
createFunctionCallAst (row, TokenName functionName : TokenLeft Rounded : TokenNumber n : ts) = FuncCall functionName (buildFunctionCall (prepareFunctionCall(row, removeOuterBrackets(formatFunctionCall(TokenLeft Rounded : TokenNumber n : ts)))))
createFunctionCallAst (row, TokenName functionName : TokenLeft Rounded : TokenLeft Rounded : TokenNumber n : ts) = FuncCall functionName (buildFunctionCall (prepareFunctionCall(row, removeOuterBrackets(formatFunctionCall(TokenLeft Rounded : TokenLeft Rounded : TokenNumber n : ts)))))
createFunctionCallAst (row, TokenName functionName : TokenLeft Rounded : ts) = FuncCall functionName (buildFunctionCall (prepareFunctionCall(row, removeOuterBrackets(formatFunctionCall(TokenLeft Rounded : ts)))))
createFunctionCallAst (row, a:as) = createFunctionCallAst (row, as)

{-
    Prepare the format from function call for the buildFunctionCall function.

    (RowNumber, [[Token]]) -> [(RowNumber, [Token])]
-}
prepareFunctionCall :: (RowNumber, [[Token]]) -> [(RowNumber, [Token])]
prepareFunctionCall (r, t) = map (\x -> (r, x)) t
--prepareFunctionCall (rowNum, tokens) = zip (repeat rowNum) tokens

{-
    Build the function call.
-}
buildFunctionCall :: [(RowNumber, [Token])] -> FuncArgs
buildFunctionCall = map processFunctionCall

{-
    Process the function call.
-}
processFunctionCall :: (RowNumber, [Token]) -> VerseExp
-- Choices
processFunctionCall (row, TokenLeft Rounded : (TokenNumber n) : (TokenPrimitives "|") : ts) = assembleChoice (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives "|" : ts)
processFunctionCall (row, (TokenNumber from) : (TokenPrimitives "..") : (TokenNumber to) : ts) = assembleChoice (row, TokenNumber from : TokenNumber to : ts)
-- Array long form syntax
processFunctionCall (row, TokenKey "array" : TokenLeft Curly : ts) = assembleTuple (row, TokenLeft Curly : ts)
-- Tuples
--processFunctionCall (row, TokenLeft Rounded : (TokenNumber n) : ts) = assembleTuple (row, TokenLeft Rounded : TokenNumber n : ts)
-- If 
processFunctionCall (row, (TokenKey "if") : ts) = assembleIfExpression (row, ts)
-- Index Access Method
processFunctionCall (row, TokenName name : TokenLeft Squared : ts) = assembleIndexAccessMethodExpression (row, TokenName name : TokenLeft Squared : ts)
-- binary operation
processFunctionCall (row, (TokenNumber n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenNumber n : TokenPrimitives op : ts)
processFunctionCall (row, (TokenLeft Rounded) : (TokenNumber n) : (TokenPrimitives op) : ts) = assembleBinaryOperation (row, TokenLeft Rounded : TokenNumber n : TokenPrimitives op : ts)
-- Int value
processFunctionCall (row, TokenNumber n : ts) = IntLit n
-- variable
processFunctionCall (row, TokenName name : ts) = Var name
processFunctionCall (row, t:ts) = processFunctionCall (row, ts)

{-
    Format the Function Call expression.
-}
formatFunctionCall :: [Token] -> [[Token]]
formatFunctionCall t = assembleExpression t []
    where 
        assembleExpression [] [] = []
        assembleExpression [] acc = [reverse acc]
        assembleExpression (TokenPrimitives "," : ts) acc = reverse acc : assembleExpression ts []
        assembleExpression (t : ts) acc = assembleExpression ts (t : acc)



-- Optimizer: Parser Feature
-- Kills every unnecessary-line (that doesn't contain a func-Def or assignment) except the last one  
-- [TokenNumber 1, TokenNewLine, TokenNumber 2, TokenSemicolon, TokenNumber 3]
-- [TokenNumber 3]

optimizer :: [Token] -> [Token]
optimizer [] = []