{-
    Project:    Verse-Interpreter
    File:       Evaluator.hs
-}

module Util.Evaluate.Evaluator
    ( evaluate
    ) where

import qualified Data.Map as Map  

import Data.List
import Util.Evaluate.EvaluatorAdditionals 
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Datatypes.Types as T
import Util.Datatypes.FalseValue
import qualified Util.Interface.ResultValue as R



evaluate :: Env -> VerseExp -> (Env, R.Result)
evaluate env (IntLit n) = (env, R.Int n)

evaluate env (StringLit n) = (env, R.String n)
-- =====Add for LP4 Logic Programmin Michael Füby
evaluate env (OwnType n t) = (env, R.String n)

evaluate env (Ast.Type t) = (env, R.Type t)

evaluate env Ast.Fail = (env, R.Fail) 

evaluate env (Ast.Tuple collection)
    | null collection = (env, R.Tuple [])
    | R.Fail `elem` map (getResult . evaluate env) collection = (env, R.Fail)
    | containsChoice collection = evaluate env (Ast.Tuple (map Ast.Tuple (allCombinations (filter (/= []) (assembleTuple collection)))))
    | otherwise = (env, R.Tuple (map (getResult . evaluate env) collection))
    where
        containsChoice (Ast.Choice _:cx) = True
        containsChoice (c:cx) = containsChoice cx
        containsChoice (c) = False
        assembleTuple ((Ast.Choice c):xs) = [c] ++ assembleTuple xs
        assembleTuple (x:xs) = [x] : assembleTuple xs
        assembleTuple (x) = [x]
        allCombinations [] = [[]]
        allCombinations (x:xs) = [y:ys | y <- x, ys <- allCombinations xs]
  

evaluate env (Ast.IAM exp index) = do 
    i <- evaluate env index
    case (exp, i) of
        (Var var, R.Int n) ->
            case Map.lookup var (variables env) of
                Just ast -> do
                    res <- evaluate env ast
                    case res of
                        (R.Tuple c) 
                            | length c - 1 < n -> (env, R.Fail)
                            | otherwise -> (env, c !! n)
                Nothing -> error ("Variable " ++ var ++ " not defined!")
        (Ast.Tuple c, R.Int n)
            | length c - 1 < n -> (env, R.Fail)
            | otherwise -> evaluate env (c !! n)
        (Ast.Tuple t, R.Choice c) -> 
            (env, R.Choice (map (getResult . ((evaluate env . Ast.IAM (Ast.Tuple t) . IntLit) . R.toInt)) c))

evaluate env (Ast.Choice c) = (env, R.Choice (map (getResult . evaluate env) c))

evaluate env (Ast.Var var) = 
    case Map.lookup var (variables env) of
        Just (ast) -> evaluate env ast
            -- | containsNode ast (Ast.Var var) -> error "juhu"
            -- | otherwise -> evaluate env ast
        Nothing -> error $ "Undefined Variable: " ++ var

evaluate env (Initialization var exp) = (Env (Map.insert var exp (variables env)) (functions env), R.Nothing)

evaluate env (Declaration var t) = (Env (Map.insert var (Ast.Type t) (variables env)) (functions env), R.Var var)

-- Concat the Strings
evaluate env (Ast.ConcatString e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
         -- =====Add for LP4 Logic Programmin Michael Füby
        (R.String lhs, R.String rhs) -> (env, R.String (rhs ++ rhs))

evaluate env (Ast.Add e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs + rhs))
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [R.Int (e + rhs) | e <- map R.toInt lhs])
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [R.Int (e + lhs) | e <- map R.toInt rhs])
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)

evaluate env (Ast.Sub e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs - rhs))
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [R.Int (e - rhs) | e <- map R.toInt lhs])
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [R.Int (e - lhs) | e <- map R.toInt rhs])
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)

evaluate env (Ast.Mul e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs * rhs))
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [R.Int (e * rhs) | e <- map R.toInt lhs])
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [R.Int (e * lhs) | e <- map R.toInt rhs])
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)

evaluate env (Ast.Lt e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs < rhs then (env, R.Int lhs) else (env, R.Fail)
        -- =====Add for LP4 Logic Programmin Michael Füby
        (R.String lhs, R.String rhs) -> if lhs < rhs then (env, R.String lhs) else (env, R.Fail)

evaluate env (Ast.Gt e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs > rhs then (env, R.Int lhs) else (env, R.Fail)
        -- =====Add for LP4 Logic Programmin Michael Füby
        (R.String lhs, R.String rhs) -> if lhs > rhs then (env, R.String lhs) else (env, R.Fail)

evaluate env (Ast.Eq e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs == rhs then (env, R.Int lhs) else (env, R.Fail)
        -- =====Add for LP4 Logic Programmin Michael Füby
        (R.String lhs, R.String rhs) -> if lhs == rhs then (env, R.String lhs) else (env, R.Fail)


evaluate env (Ast.And e1 e2) = do
    case (e1, e2) of
        (Initialization var (Ast.Choice c), e2) -> do
            let varSet = zip (repeat var) c
            let eval = map getResult (map (\var -> evaluate (Env {variables = Map.union (Map.fromList [var]) (variables env), functions = functions env}) e2) varSet)
            let newVar = (var, Choice (filter (/= Ast.Fail) (map resToAst eval)))
            (Env {variables = Map.union (Map.fromList [newVar]) (variables env), functions = functions env}, R.Var var)
        (e1, e2) -> do
            lhs <- evaluate env e1
            rhs <- evaluate env e2
            case (lhs, rhs) of
                (R.Fail, R.Fail) -> (env, R.Fail)
                (lhs, R.Fail) -> (env, R.Fail)
                (R.Fail, rhs) -> (env, R.Fail)
                (lhs, rhs) -> (env, lhs)

evaluate env (Ast.Or e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Fail, R.Fail) -> (env, R.Fail)
        (lhs, R.Fail) -> (env, lhs) 
        (R.Fail, rhs) -> (env, rhs) 
        (lhs, rhs) -> (env, lhs) 

evaluate env (Ast.Question e) = do
    lhs <- evaluate env e
    case lhs of 
        (R.Tuple c) -> (env, R.Choice c)

evaluate env (Ast.If e1 e2 e3) = do
    c <- evaluate env e1
    case c of
        R.Fail -> evaluate env e3
        c -> evaluate env e2

evaluate env (Ast.BasicFor e) = do
    lhs <- evaluate env e
    case lhs of 
        (R.Choice c) -> (env, R.Tuple c)
        (R.Int n) -> (env, R.Tuple [R.Int n])
        (R.Fail) -> (env, R.Tuple [])
            
evaluate env (Ast.ForDo domain range) = do
    case (domain, range) of 
        (Initialization var (Ast.Choice c), range) -> do
            let varSet = zip (repeat var) c
            let result = map getResult (map (\var -> evaluate (Env {variables = Map.union (Map.fromList [var]) (variables env), functions = functions env}) range) varSet)
            ((Env {variables = (variables env), functions = functions env}), R.Tuple result)   
        (Declaration var T.Int, range) -> do
             let soulutions = filter (\x -> fst x /= R.Fail) (loop 0 R.Nothing range)
                                where
                                    loop n R.Fail ast = []
                                    loop n res ast = do
                                        let newEnv = Env {variables = Map.union (Map.fromList [(var, IntLit n)]) (variables env), functions = functions env}
                                        let result = getResult (evaluate newEnv ast)
                                        (result, n) : loop (n+1) result ast
             evaluate env (Initialization var (Ast.Choice (map (IntLit . snd) soulutions)))
             (env, R.Tuple (map fst soulutions))
        (domain, range) -> error (show domain) 

evaluate env (Ast.FuncDef name params body return) = (Env (variables env) (Map.insert name (params, body, return) (functions env)), R.Fail)

evaluate env (Ast.FuncCall name args) = do
    let evalArgs = map (getResult . evaluate env) args 
    case Map.lookup name (functions env) of
        Just (params, body, return) 
            | length params > length args -> error "Too few arguments provided" 
            | length params < length args -> error "Too much arguments provided" 
            | not (isValidFuncArgType params evalArgs) -> error "Invalid Type" 
            | otherwise -> do 
                let evalArgs = map ((resToAst . getResult) . evaluate env) args
                let varList = Map.fromList (zip (getParamNameList params) evalArgs)
                let newEnv = Env { variables = Map.union varList (variables env), functions = functions env }
                let (bodyEnv, bodyRes) = evaluate newEnv body
                evaluate bodyEnv (Ast.Unify (Ast.Type return) (resToAst bodyRes))
        Nothing -> error ("Function " ++ show name ++ " not defined!")

evaluate env (Ast.Unify lhs rhs) = do
    case (lhs, rhs) of
        (Ast.Var var, e) -> do
            let (env', res') = evaluate env lhs
            let (env'', res'') = evaluate env rhs
            case (res', res'') of
                (R.Type T.Int, R.Int n) ->
                    let (newEnv, res) = evaluate env (Initialization var (IntLit n))
                    in (newEnv, R.Int n)
                (R.Type (T.TupleFixed t1), R.Tuple t2) -> do
                    let collection = getCollection rhs
                            where
                                getCollection (Ast.Tuple c) = c
                    let evalCollection = map (evaluate env) collection
                    let vars = Map.union (Map.fromList [(var, Ast.Tuple collection)]) (Map.unions (map (variables . getEnvironment) evalCollection))
                    let newEnv = Env {variables = vars, functions = functions env}
                    (newEnv, R.Tuple t2)
                (R.Tuple t1, R.Tuple t2) -> do
                    case Map.lookup var (variables env) of
                        Just ast -> do 
                            case (ast, rhs) of
                                (Ast.Tuple c1, Ast.Tuple c2) -> do
                                    let vars = executePartialValues (zip c1 c2)
                                            where 
                                                executePartialValues [] = []
                                                executePartialValues ((Declaration var t, res):xs) = (var, res) : executePartialValues xs
                                                executePartialValues ((res, Declaration var t):xs) = (var, res) : executePartialValues xs
                                                executePartialValues (x:xs) = executePartialValues xs
                                    let unitedVars = Map.union (Map.fromList vars) (variables env)  
                                    let newEnv = Env {variables = unitedVars, functions = functions env}
                                    let newTupleForVar = Ast.Tuple (assembleTuple c1)
                                            where 
                                                assembleTuple [] = []
                                                assembleTuple (Declaration var t:xs) = do 
                                                    case Map.lookup var unitedVars of
                                                        Just res -> res : assembleTuple xs 
                                                        Nothing -> Declaration var t : assembleTuple xs
                                                assembleTuple (x:xs) = x : assembleTuple xs
                                    let newEnv' = Env {variables = Map.insert var newTupleForVar (variables newEnv), functions = functions newEnv}
                                    (newEnv', getResult (evaluate newEnv' newTupleForVar))
                        Nothing -> (env, R.Fail)
                (_, _) -> (env, R.Fail)
        (e, Ast.Var var) -> evaluate env (Ast.Unify (Ast.Var var) e)
        (Ast.Type T.Int, Ast.IntLit n) -> (env, R.Int n)
        (Ast.IntLit n, Ast.Type T.Int) -> (env, R.Int n)
        (Ast.Type T.Int, Ast.Type T.Int) -> (env, R.Type T.Int)
        (IntLit n1, IntLit n2)
            | n1 == n2 -> (env, R.Int n1)
            | otherwise -> (env, R.Fail)
        (e1, e2) 
            | e1 == e2 -> (env, R.Structure e1)
            | otherwise -> (env, R.Fail)




