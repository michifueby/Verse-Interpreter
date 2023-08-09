{-
    Project:    Verse-Interpreter
    File:       EvaluatorAdditionals.hs
-}

module Util.Evaluate.EvaluatorAdditionals where

import qualified Util.Interface.ResultValue as R
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Datatypes.Types as T ( Type(..))
import qualified Data.Map as Map  
import Util.Shared ()
import Util.Evaluate.Environment (Env)


{- 
    -------------------------------------------
    |         Evaluate Additionals            |
    -------------------------------------------
-}

{-
    Search and replace element from list.
-}
searchAndReplace :: Eq t => t -> t -> [t] -> [t]
searchAndReplace _ _ [] = []
searchAndReplace old new (x:xs)
    | x == old  = new : searchAndReplace old new xs
    | otherwise = x : searchAndReplace old new xs

{-
    Get VerseExp from Result.
-}
resToAst :: R.Result -> VerseExp
resToAst (R.Int n) = Ast.IntLit n
resToAst (R.Type t) = Ast.Type t
resToAst (R.Choice c) = Ast.Choice (map resToAst c)
resToAst (R.Tuple t) = Ast.Tuple (map resToAst t)
resToAst (R.Var var) = Ast.Var var
resToAst (R.String s) = Ast.String s
resToAst (R.CustomType tName fields) = Ast.TypeData tName (Map.fromList (map (\(k, v) -> (k, resToAst v)) fields))
resToAst R.Fail = Ast.Fail

{-
    Get collection from VerseExp.
-}
getCollection :: VerseExp -> [VerseExp]
getCollection (Ast.Choice c) = c
getCollection (Ast.Tuple t) = t

{-
    Get type of result.
-}
typeOf :: R.Result -> T.Type
typeOf (R.Int n) = T.Int
typeOf (R.String s) = T.String

{-
    Get is typeof result.
-}
isTypeOf :: R.Result -> T.Type -> Bool
isTypeOf res t
  | typeOf res == t = True
  | otherwise = False

{-
    Get is a valid func arg type.
-}
isValidFuncArgType :: Parameters -> [R.Result] -> Bool
isValidFuncArgType params = check (getParamTypeList params)
  where 
    check (T.Int:rs) (R.Int _:ps) = check rs ps
    check (T.Int:rs) (R.Type T.Int:ps) = check rs ps
    check (T.String:rs) (R.String _:ps) = check rs ps 
    check (T.String:rs) (R.Type T.String:ps) = check rs ps 
    check [] [] = True
    check _ _ = False

{-
    Represents the Predicate.
-}
type Predicate = VerseExp

{-
    Check if node contains an element.
-}
containsNode :: VerseExp -> Predicate -> Bool
containsNode (IntLit n) predicate   
    | IntLit n == predicate = True
    | otherwise = False
containsNode (Ast.Var var) predicate
    | Ast.Var var == predicate = True
    | otherwise = False
containsNode (Ast.Add e1 e2) predicate  
    | Ast.Add e1 e2 == predicate = True
    | otherwise = do 
        let left = containsNode e1 predicate  
        let right = containsNode e2 predicate 
        case (left, right) of
            (False, False) -> False
            (_, _) -> True

{-
    Get the enviroment.
-}
getEnvironment :: (Env, R.Result) -> Env
getEnvironment = fst

{-
    Get the result.
-}
getResult :: (Env, R.Result) -> R.Result
getResult = snd

