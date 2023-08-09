{-
    Project:    Verse-Interpreter
    File:       EnviromentAdditionals.hs
-}

module Util.Evaluate.EnvironmentAdditionals where

import qualified Data.Map as Map
import qualified Util.Interface.AbstractSyntaxTree as Ast
import qualified Util.Datatypes.Types as T

import Util.Shared ( Name )
import Util.Evaluate.Environment
import Util.Core.CoreFunctions


{- 
  ---------------------------------------
  |           Generate Function         |                
  ---------------------------------------
-}

{-
  Create a empty enviroment for the evaluator.
-}
createEmptyEnvironment :: Env
createEmptyEnvironment = Env {variables = Map.empty, functions = Map.empty, types = Map.empty}

{-
  Create a new enviroment for the evaluator.
-}
createNewEnvironment :: Vars -> Func -> Types -> Env
createNewEnvironment vars funcs customTypes = Env {variables = vars, functions = funcs, types = customTypes}

{- 
    --------------------------------------
    |           Access Functions         |                
    --------------------------------------
-}

{-
    Access Variables
-}
getVarName :: Env -> Name -> Either String Name
getVarName env varName =
  case Map.lookup varName (variables env) of
    Just exp -> Right varName
    Nothing -> Left $ "Could not locate Variable " ++ show varName

getVarExp :: Env -> Name -> Either String Expression
getVarExp env varName =
  case Map.lookup varName (variables env) of
    Just exp -> Right exp
    Nothing -> Left $ "Could not locate Variable " ++ show varName

{-
   Access Functions
-}
getFuncName :: Env -> Name -> Either String Name
getFuncName env funcName = 
  case Map.lookup funcName (functions env) of
    Just funcMeta -> Right funcName
    Nothing -> Left $ "Could not locate Function " ++ show funcName

getFuncMetadata :: Env -> Name -> Either String (Ast.Parameters, Ast.FuncBody, T.Type)
getFuncMetadata env funcName = 
  case Map.lookup funcName (functions env) of
    Just funcMeta -> Right funcMeta
    Nothing -> Left $ "Could not locate Function " ++ show funcName

getFuncParameters :: Env -> Name -> Either String Ast.Parameters
getFuncParameters env funcName = 
  case getFuncMetadata env funcName of
    Right funcMeta -> Right $ extractParams funcMeta
      where 
        extractParams (p, _, _) = p
    Left msg -> Left $ "Could not locate Function " ++ show funcName

getFuncBody :: Env -> Name -> Either String Ast.FuncBody
getFuncBody env funcName = 
  case getFuncMetadata env funcName of
    Right funcMeta -> Right $ extractBody funcMeta
      where 
        extractBody (_, b, _) = b
    Left msg -> Left $ "Could not locate Function " ++ show funcName

getFuncReturn :: Env -> Name -> Either String T.Type
getFuncReturn env funcName = 
  case getFuncMetadata env funcName of
    Right funcMeta -> Right $ extractReturn funcMeta
      where 
        extractReturn (_, _, r) = r
    Left msg -> Left $ "Could not locate Function " ++ show funcName

{-
    Access Types
-}
getTypeName :: Env -> Ast.TypeName -> Either String Ast.TypeName
getTypeName env typeName = 
    case Map.lookup typeName (types env) of
        Just fields -> Right typeName
        Nothing -> Left $ "Could not Locate Type " ++ show typeName 

getTypeFields :: Env -> Ast.TypeName -> Either String (Map.Map Ast.FieldName Data)
getTypeFields env typeName =
  case Map.lookup typeName (types env) of
    Just fields -> Right fields
    Nothing -> Left $ "Could not Locate Type " ++ show typeName 

getFieldName :: Env -> Ast.TypeName -> Ast.FieldName -> Either String Ast.FieldName
getFieldName env typeName fieldName = 
  case getTypeFields env typeName of
    Right fields -> 
      case Map.lookup fieldName fields of
        Just field -> Right fieldName 
        Nothing -> Left $ "Could not Locate " ++ show fieldName ++ " on " ++ show typeName
    Left msg -> Left $ "Could not Locate Type " ++ show typeName

getFieldData :: Env -> Ast.TypeName -> Ast.FieldName -> Either String Data
getFieldData env typeName fieldName = 
  case getTypeFields env typeName of
    Right fields -> 
      case Map.lookup fieldName fields of
        Just field -> Right field 
        Nothing -> Left $ "Could not Locate " ++ show fieldName ++ " on " ++ show typeName
    Left msg -> Left $ "Could not Locate Type " ++ show typeName

{- 
    --------------------------------------
    |           Insert Functions         |                
    --------------------------------------
-}

insertVar :: Env -> (Name, Expression) -> Env
insertVar env (varName, exp) = Env (Map.insert varName exp (variables env)) (functions env) (types env)

insertVars :: Env -> [(Name, Expression)] -> Env
insertVars env vars = foldl (\newEnv acc -> Env (Map.insert (fst acc) (snd acc) (variables newEnv)) (functions newEnv) (types newEnv)) env vars
  
insertFunc :: Env -> (Name, (Ast.Parameters, Ast.FuncBody, T.Type)) -> Env
insertFunc env (funcName, (params, body, return)) = Env (variables env) (Map.insert funcName (params, body, return) (functions env)) (types env)

insertFuncs :: Env -> [(Name, (Ast.Parameters, Ast.FuncBody, T.Type))] -> Env
insertFuncs env funcs = foldl (\newEnv acc -> Env (variables newEnv) (Map.insert (fst acc) (snd acc) (functions newEnv)) (types newEnv)) env funcs

insertType :: Env -> (Name, Map.Map Ast.FieldName Data) -> Env
insertType env (typeName, fields) = Env (variables env) (functions env) (Map.insert typeName fields (types env)) 

insertTypes :: Env -> [(Name, Map.Map Ast.FieldName Data)] -> Env
insertTypes env customTypes = foldl (\newEnv acc -> Env (variables newEnv) (functions newEnv) (Map.insert (fst acc) (snd acc) (types newEnv))) env customTypes

insertField :: Env -> (Name, (Ast.FieldName, Data)) -> Either String Env
insertField env (typeName, (fieldName, fieldData)) = 
  case Map.lookup typeName (types env) of
    Just fields -> Right $ Env (variables env) (functions env) (Map.insert typeName (Map.insert fieldName fieldData fields) (types env))
    Nothing -> Left $ "Could not Locate Type " ++ show typeName

{- 
    -------------------------------------
    |           Union Functions         |                
    -------------------------------------
-}

unionVars :: Env -> Vars -> Env
unionVars env vars = Env {variables = Map.union vars (variables env), functions = functions env, types = types env} 

unionFuncs :: Env -> Func -> Env
unionFuncs env func = Env {variables = variables env, functions = Map.union func (functions env), types = types env} 

unionTypes :: Env -> Types -> Env
unionTypes env customTypes = Env {variables = variables env, functions = functions env, types = Map.union customTypes (types env)} 

mergeEnv :: [Env] -> Env
mergeEnv envs = Env mergedVars mergedFuncs mergedTypes
  where
    mergedVars = foldl mergeVarsMap Map.empty (map variables envs)
    mergedFuncs = foldl mergeFuncsMap Map.empty (map functions envs)
    mergedTypes = foldl mergeTypesMap Map.empty (map types envs)

    mergeVarsMap :: Vars -> Vars -> Vars
    mergeVarsMap = Map.union

    mergeFuncsMap :: Func -> Func -> Func
    mergeFuncsMap = Map.union

    mergeTypesMap :: Types -> Types -> Types
    mergeTypesMap = Map.union

{- 
    ---------------------------------------
    |           Extract Functions         |                
    ---------------------------------------
-}

extractParams :: (Ast.Parameters, Ast.FuncBody, T.Type) -> Ast.Parameters
extractParams (p, _, _) = p 

extractBody :: (Ast.Parameters, Ast.FuncBody, T.Type) -> Ast.FuncBody
extractBody (_, b, _) = b

extractReturn :: (Ast.Parameters, Ast.FuncBody, T.Type) -> T.Type
extractReturn (_, _, r) = r