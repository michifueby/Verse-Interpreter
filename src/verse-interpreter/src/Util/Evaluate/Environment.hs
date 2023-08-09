{-
    Project:    Verse-Interpreter
    File:       Environment.hs
-}

module Util.Evaluate.Environment where


import qualified Data.Map as Map
import qualified Util.Interface.AbstractSyntaxTree as Ast
import qualified Util.Datatypes.Types as T

import Util.Shared ( Name )

{- 
    ------------------------------------------
    |              Environment               |                
    ------------------------------------------
-}

{-
    Data.
-}
type Data = Ast.Exp

{-
    Expression.
-}
type Expression = Ast.Exp

{-
    Variables.
-}
type Vars = Map.Map Name Expression

{-
    Functions.
-}
type Func = Map.Map Ast.FuncName (Ast.Parameters, Ast.FuncBody, T.Type)

{-
    Custom Types.
-}
type Types = Map.Map Ast.TypeName (Map.Map Ast.FieldName Data)

{-
    Environment-Table.
-}
data Env = Env { variables :: Vars, functions :: Func, types :: Types}

{- 
    ------------------------------------------
    |           Environment Instance         |                
    ------------------------------------------
-}
instance Semigroup Env where
  Env v1 f1 t1 <> Env v2 f2 t2 = Env (Map.union v1 v2) (Map.union f1 f2) (Map.union t1 t2)

instance Monoid Env where
  mempty = Env Map.empty Map.empty Map.empty

instance Show Env where
    show (Env vars funcs types) = "Variables: \n\t" ++ showVarList (Map.toList vars) ++ "\n" ++
                                  "Functions: \n\t" ++ showFuncList (Map.toList funcs) ++ "\n" ++
                                  "CustomTypes: \n\t" ++ showTypeList (Map.toList types) ++ "\n" 

{-
    Show instance for environment.
-}
showVar :: (String, Ast.Exp) -> String
showVar (var, exp) = var ++ " = " ++ show exp

{-
    Show for var list.
-}
showVarList :: [(String, Ast.Exp)] -> String
showVarList [] = ""
showVarList [x] = showVar x
showVarList (x:xs) = showVar x ++ "\n\t" ++ showVarList xs 

{-
    Show for function.
-}
showFunc :: (Ast.FuncName, (Ast.Parameters, Ast.FuncBody, T.Type)) -> String
showFunc (var, exp) = var ++ "(" ++ showParams (getFuncParameters exp) ++ "):" ++ show (getFuncReturn exp) ++ " := " ++ show (getFuncBody exp) 
  where 
    getFuncParameters (p, _, _) = p
    getFuncBody (_, b, _) = b
    getFuncReturn (_, _, r) = r
    showParams [] = []
    showParams [p] = getParameterName p ++ ":" ++ show (getParameterType p)
    showParams (p:ps) = getParameterName p ++ ":" ++ show (getParameterType p) ++ ',' : showParams ps
    getParameterName (name, _) = name
    getParameterType (_, t) = t           

{-
    Show for function list.
-} 
showFuncList :: [(Ast.FuncName, (Ast.Parameters, Ast.FuncBody, T.Type))] -> String
showFuncList [] = ""
showFuncList [x] = showFunc x
showFuncList (x:xs) = showFunc x ++ "\n\t" ++ showFuncList xs

{-
    Show for type.
-}
showType :: (Ast.TypeName, Map.Map Ast.FieldName Data) -> String
showType (typeName, typeFields) = typeName ++ " = { \n\t" ++ showFields (Map.toList typeFields) 
    where
        showFields [] = "}"
        showFields [x] = "   " ++ fst x ++ " = " ++ show (snd x) ++ "\n\t}"
        showFields (x:xs) = "   " ++ fst x ++ " = " ++ show (snd x) ++ "\n\t" ++ showFields xs 

{-
    Show for type list.
-}
showTypeList :: [(Ast.TypeName, Map.Map Ast.FieldName Data)] -> String
showTypeList [] = ""
showTypeList [x] = showType x
showTypeList (x:xs) = showType x ++ "\n\t" ++ showTypeList xs 

