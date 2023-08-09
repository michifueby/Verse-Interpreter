{-
    Project:    Verse-Interpreter
    File:       AbstractSyntaxTree.hs
-}

module Util.Interface.AbstractSyntaxTree where

import Data.List (intercalate)
import Util.Shared
import Util.Datatypes.IntegerValue
import qualified Util.Datatypes.Types as T

{-
    Show a name list as a string
-}
showNameList :: [Name] -> String
showNameList ns = "(" ++ unwords ns ++ ")" ++ ")"


-- Var
type VarName = Name
type Exp = VerseExp

-- Equation
type Lhs = VerseExp
type Rhs = VerseExp

-- If - Statement
type Condition = VerseExp
type Then = VerseExp
type Else = VerseExp

-- For - Statement & ?-Operator
type Domain = VerseExp
type Range = VerseExp

-- Index Access
type Collection = VerseExp
type Index = VerseExp

-- Functions
type ParamName = Name
type FuncName = Name
type Parameter = (ParamName, T.Type)
type Parameters = [Parameter]
type FuncBody = VerseExp
type Return = T.Type
type FuncArgs = [VerseExp]

getParamName :: Parameter -> ParamName
getParamName = fst

getParamType :: Parameter -> T.Type
getParamType = snd

getParamNameList :: Parameters -> [ParamName]
getParamNameList = map getParamName

getParamTypeList :: Parameters -> [T.Type]
getParamTypeList = map getParamType


{-
    Declare the Expression for the AST
-}
data VerseExp = IntLit Int                                  -- Literals eg. 4, 20, 13
            | Type T.Type                                   -- types int
            | Fail                                          -- Fail 
            | Tuple [Exp]                                   -- Tuples ()
            | IAM Collection Index                          -- Index Access
            | Choice [Exp]                                  -- Choices |
            | Var Name                                      -- Variable name "test", "x", "y"
            | StringLit String     -- =====Add for LP4 Logic Programmin Michael Füby
            | Declaration VarName T.Type                    -- Variable Deklaration "x:int"
            | Initialization VarName Exp                    -- Variable Initialization `x:=2`
            | Add Lhs Rhs                                   -- Addition `+`
            | ConcatString Lhs Rhs                          -- String Concat `++`   -- =====Add for LP4 Logic Programmin Michael Füby
            | Sub Lhs Rhs                                   -- Subtraction `-`
            | Mul Lhs Rhs                                   -- Multiplication `*`
            | Lt Lhs Rhs                                    -- Less Than `<`
            | Gt Lhs Rhs                                    -- Greater Than `>`
            | Eq Lhs Rhs                                    -- Equal `=`
            | And Lhs Rhs                                   -- Logical `AND` 
            | Or Lhs Rhs                                    -- Logical `OR`
            | Question Exp                                  -- ToChoice `?` 
            | If Condition Then Else                        -- If-Expression (if then else)
            | BasicFor Domain                               -- For with Domain only
            | BasicForAdditionals [Domain]                  -- For with Domain only
            | ForDo Domain Range                            -- For with Domain & Range
            | FuncDef FuncName Parameters FuncBody Return   -- Function Definition f(x:int):int := x+1;
            | FuncCall FuncName FuncArgs                    -- Function Call f(2,3) 
            | Unify Lhs Rhs
            | OwnType Name [VerseExp]
            deriving (Eq, Ord)

-- Create instance - Show
instance Show VerseExp where
    show (IntLit n) = show n  
    show (Type t) = show t
    show (Var m) = m         
    show (Initialization left right) = showInitialization left right                                                                                            
    show (Declaration left right) = show left ++ ":" ++ show right                                   
    show (Tuple c) = "Tuple: " ++ show c        
    show (IAM c index) = show c ++ "[" ++ show index ++ "]"
    show (Choice c) = "Choice: " ++ show c              
    show (Add left right) = show left ++ " + " ++ show right    
    show (Sub left right) = show left ++ " - " ++ show right 
    show (Mul left right) = show left ++ " * " ++ show right 
    show (Lt left right) =  show left ++ " < " ++ show right 
    show (Gt  left right) = show left ++ " > " ++ show right 
    show (Eq left right) = show left ++ " = " ++ show right 
    show (And left right) = showParsingExpression "," [left, right]
    show (Or left right) = showParsingExpression "|" [left, right]
    show (If conditionExpression thenExpression elseExpression) = "if (" ++ show conditionExpression ++ ") then " ++ show thenExpression ++ " else " ++ show elseExpression
    show (FuncDef name args body return) = name ++ "(" ++ showParams args ++ "):" ++ show return ++ " := " ++ show body
    show (FuncCall name args) = name ++ "(" ++ show args ++ ")"
    show (Question c) = show c ++ "?"
    show (BasicFor domain) = show "for" ++ "{" ++ show domain ++ "}"
    show (BasicForAdditionals domain) = show "for" ++ "{" ++ show domain ++ "}"
    show Fail = "false?"
    show (Unify lhs rhs) = "Unification:" ++ show lhs ++ "=" ++ show rhs
    show (ForDo domain range) = "ForDo" ++ "(" ++ show domain ++ ")" ++ " do" ++ "(" ++ show range ++ ")"
    show (StringLit n) = "String " ++ "\"" ++ show n ++ "\""  -- =====Add for LP4 Logic Programmin Michael Füby
    show (OwnType name list) = "OwnType: " ++ show name ++ "\"" ++ show list ++ "\""  -- =====Add for LP4 Logic Programmin Michael Füby

showParams :: Parameters -> String
showParams [p] = fst p ++ ":" ++ show (snd p)
showParams (p:ps) = fst p ++ ":" ++ show (snd p) ++ ", " ++ showParams ps 

{- 
    Shows the parsing expression as string
-}
showParsingExpression :: String -> [VerseExp] -> String
showParsingExpression op es = "(" ++ op ++ " " ++ showExpressionList es ++ ")"

showInitialization :: String -> VerseExp -> String
showInitialization name expression = name ++ " " ++ show expression 

{- 
    Show expression list as string
    -- https://hoogle.haskell.org/?hoogle=intercalate
    -- https://hoogle.haskell.org/?hoogle=unwords
-}
showExpressionList :: [VerseExp] -> String
showExpressionList es = unwords (map show es)

-- Examples:
-- IntLit 1                                Output: 1
-- Var "x"                                 Output: x
-- Add (IntLit 1) (IntLit 1)               Output: 1+1
-- Add (Var "x") (IntLit 3)                Output: x + 3