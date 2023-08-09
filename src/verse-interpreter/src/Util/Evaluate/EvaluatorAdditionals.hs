{-
    Project:    Verse-Interpreter
    File:       EvaluatorAdditionals.hs
-}

module Util.Evaluate.EvaluatorAdditionals where

import Util.Interface.ResultValue as R
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Datatypes.Types as T
import qualified Data.Map as Map  

type Vars = Map.Map String Ast.Exp

type Func = Map.Map Ast.FuncName (Ast.Parameters, Ast.FuncBody, T.Type)

data Env = Env { variables :: Vars, functions :: Func}

getFuncParameters :: (Ast.Parameters, Ast.FuncBody, T.Type) -> Ast.Parameters
getFuncParameters (p, b, r) = p  

getFuncBody :: (Ast.Parameters, Ast.FuncBody, T.Type) -> Ast.FuncBody
getFuncBody (p, b, r) = b

getFuncReturn :: (Ast.Parameters, Ast.FuncBody, T.Type) -> T.Type
getFuncReturn (p, b, r) = r  


showVar :: (String, Ast.Exp) -> String
showVar (var, exp) = var ++ " = " ++ show exp

showVarList :: [(String, Ast.Exp)] -> String
showVarList [] = ""
showVarList [x] = showVar x
showVarList (x:xs) = showVar x ++ "\n\t" ++ showVarList xs 

showFunc :: (Ast.FuncName, (Ast.Parameters, Ast.FuncBody, T.Type)) -> String
showFunc (var, exp) = var ++ "(" ++ showParams (getFuncParameters exp) ++ "):" ++ show (getFuncReturn exp) ++ " := " ++ show (getFuncBody exp) 
  where 
    showParams [] = []
    showParams [p] = getParamName p ++ ":" ++ show (getParamType p)
    showParams (p:ps) = getParamName p ++ ":" ++ show (getParamType p) ++ ',' : showParams ps  

showFuncList :: [(Ast.FuncName, (Ast.Parameters, Ast.FuncBody, T.Type))] -> String
showFuncList [] = ""
showFuncList [x] = showFunc x
showFuncList (x:xs) = showFunc x ++ "\n\t" ++ showFuncList xs

   
instance Show Env where
    show (Env vars funcs) = "Variables: \n\t" ++ showVarList  (Map.toList vars)  ++ "\n" ++
                            "Functions: \n\t" ++ showFuncList (Map.toList funcs) ++ "\n\n"

instance Semigroup Env where
  Env v1 f1 <> Env v2 f2 = Env (Map.union v1 v2) (Map.union f1 f2)

instance Monoid Env where
  mempty = Env Map.empty Map.empty


searchAndReplace :: Eq t => t -> t -> [t] -> [t]
searchAndReplace _ _ [] = []
searchAndReplace old new (x:xs)
    | x == old  = new : searchAndReplace old new xs
    | otherwise = x : searchAndReplace old new xs

resToAst :: Result -> VerseExp
resToAst (R.Int n) = Ast.IntLit n
resToAst (R.Type t) = Ast.Type t
resToAst (R.Choice t) = Ast.Choice (map resToAst t)
resToAst (R.Var var) = Ast.Var var
resToAst R.Fail = Ast.Fail


isValidFuncArgType :: Parameters -> [Result] -> Bool
isValidFuncArgType params res = check (getParamTypeList params) res
  where 
    check (T.Int:rs) (R.Int _:ps) = check rs ps 
    check [] [] = True
    check _ _ = False

type Predicate = VerseExp

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


getEnvironment :: (Env, Result) -> Env
getEnvironment = fst

getResult :: (Env, Result) -> Result
getResult = snd