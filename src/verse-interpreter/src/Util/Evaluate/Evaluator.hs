{-
    Project:    Verse-Interpreter
    File:       Evaluator.hs
-}

module Util.Evaluate.Evaluator
    ( evaluate
    ) where


import qualified Data.Map as Map  
import qualified Util.Interface.ResultValue as R

import Data.List
import Data.Function
import Util.Evaluate.EvaluatorAdditionals 
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Datatypes.Types as T
import Util.Datatypes.FalseValue
import Util.Evaluate.Environment (Env (..))
import Util.Evaluate.EnvironmentAdditionals
import Util.Shared as SR


evaluate :: Env -> VerseExp -> (Env, R.Result)
evaluate env (IntLit n) = (env, R.Int n)

evaluate env (Ast.String s) = (env, R.String s)

evaluate env (Ast.TypeDef tName tFields) = (insertType env (tName, tFields), R.Nothing)

evaluate env (TypeData tName fields) = (env, R.CustomType tName (getFields (Map.toList fields)))
        where
            getFields [] = []
            getFields ((k, v):xs) = (k, (getResult . evaluate env) v) : getFields xs

evaluate env (Ast.FieldInstance var (TypeData tName fields))
    | Map.null fields =
        case getTypeFields env tName of
            Right fields' -> (insertVar env (var, TypeData tName fields'), R.Nothing)
            Left msg -> (env, R.Fail) 
    | otherwise = 
        case getTypeName env tName of
            Right name -> (insertVar env (var, TypeData tName fields), R.Nothing)
            Left msg -> (env, R.Fail)

evaluate env (FieldAccess var fName) = 
    case getVarExp env var of
        Right (TypeData tName tFields) -> 
            case Map.lookup fName tFields of
                Just exp -> evaluate env exp
                Nothing -> (env, R.Fail)
        Right exp -> (env, R.Fail)
        Left msg -> (env, R.Fail)  

evaluate env (FieldInitialization var fName fData) =
    case getVarExp env var of
        Right (TypeData tName tFields) -> (insertVar env (var, TypeData tName (Map.insert fName fData tFields)), R.Nothing)
        Left msg -> (env, R.Fail)

evaluate env (Ast.Type t) = (env, R.Type t)

evaluate env Ast.Fail = (env, R.Fail) 

evaluate env (Ast.Tuple collection)
    | null collection = (env, R.Tuple [])
    | R.Fail `elem` map (getResult . evaluate env) collection = (env, R.Fail)
    | otherwise = do
        let evalCollection = map (getResult . evaluate env) collection
        let res = map (\r -> case r of 
                            (R.Choice _) -> True
                            (_) -> False ) evalCollection
        case (True `elem` res) of
            False -> (env, R.Tuple evalCollection)
            True -> (env, R.Choice $ map R.Tuple $ allCombinations $ getChoice evalCollection)
                        where
                            allCombinations [] = [[]]
                            allCombinations (x:xs) = [y:ys | y <- x, ys <- allCombinations xs]
                            getChoice [] = []
                            getChoice (R.Choice c:xs) = c : getChoice xs
                            getChoice (x:xs) = [x] : getChoice xs

evaluate env (Ast.IndexAccess exp index) = do
    case (exp, index) of
        (Ast.Var var, Declaration i T.Int) ->
            case evaluate env (Ast.Var var) of
                (env, R.Tuple t) -> do
                    let newEnv = insertVar env (i, Ast.Choice $ map IntLit $ init [0..(length t)])
                    (newEnv, R.Choice t)
                (env, _) -> (env, R.Fail)      
        (Ast.Tuple t, Declaration i T.Int) -> 
            case evaluate env (Ast.Tuple t) of
                (env, R.Tuple t) -> (insertVar env (i, Ast.Choice $ map IntLit $ init [0..(length t)]), R.Choice t)
                (env, _) -> (env, R.Fail)
        (exp, index) -> do
            i <- evaluate env index
            case (exp, i) of
                (Ast.Var var, i) -> 
                    case (getResult $ evaluate env (Ast.Var var), i) of
                        (R.Tuple t, R.Int n)
                            | n < 0 -> (env, R.Fail)
                            | length t - 1 < n -> (env, R.Fail)
                            | otherwise -> (env, t !! n)
                        (R.Tuple t, R.Choice c) -> 
                            (env, R.Choice $ filter (/= R.Fail) $ map (getResult . (evaluate env . Ast.IndexAccess (resToAst (R.Tuple t)) . IntLit)) $ R.extractIntFromCollection $ R.Choice c)
                        (_, _) -> (env, R.Fail)
                (Ast.Tuple t, R.Int n)
                    | n < 0 -> (env, R.Fail)
                    | length t - 1 < n -> (env, R.Fail)
                    | otherwise -> evaluate env (t !! n)
                (Ast.Tuple t, R.Choice c) ->
                    (env, R.Choice $ filter (/= R.Fail) $ map (getResult . (evaluate env . Ast.IndexAccess (Ast.Tuple t) . IntLit)) $ R.extractIntFromCollection $ R.Choice c)


evaluate env (Ast.Choice c) = do
    let content = map evalContent c
            where 
                evalContent (Ast.Choice c) = map (getResult . evaluate env ) c
                evalContent x = [getResult $ evaluate env x]
    let flatterContent = concatMap flatter $ concat content
            where
                flatter (R.Choice c) = c
                flatter x = [x]
    case filter (/= R.Fail) flatterContent of
        [] -> (env, R.Fail)
        res -> (env, R.Choice res)

evaluate env (Seq rep seq) = 
    case rep of
        SR.One -> foldl (\acc ast -> evaluate (getEnvironment acc) ast) (env, R.Nothing) seq
        SR.All -> do
            let res = scanl (\acc ast -> evaluate (getEnvironment acc) ast) (env, R.Nothing) seq
            (getEnvironment(last res), R.AllResult (map getResult res))

evaluate env (Ast.Var var) = 
    case getVarExp env var of
        Right ast -> evaluate env ast
        Left msg -> (env, R.Fail)-- error $ "Undefined Variable: " ++ var

evaluate env (Initialization var exp) = (insertVar env (var, exp), R.Nothing)

evaluate env (Declaration var t) = (insertVar env (var, Ast.Type t), R.Nothing)

evaluate env (Ast.Add e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs + rhs))
        (R.String lhs, R.String rhs) -> (env, R.String (lhs ++ rhs))
        (R.Choice lhs, R.Choice rhs) -> (env, R.Choice [getResult (evaluate env (Add l r)) | l <- map resToAst lhs, r <- map resToAst rhs ])
        (R.Int lhs, R.String rhs) -> (env, R.String (R.toString (R.Int lhs) ++ rhs))
        (R.String lhs, R.Int rhs) -> (env, R.String (lhs ++ R.toString (R.Int rhs)))
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [getResult (evaluate env (Add (resToAst (R.Int lhs)) e)) | e <- map resToAst rhs])
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [getResult (evaluate env (Add e (resToAst (R.Int rhs)))) | e <- map resToAst lhs])
        (R.Choice lhs, R.String rhs) -> (env, R.Choice [R.String (e ++ rhs) | e <- map R.toString lhs])
        (R.String lhs, R.Choice rhs) -> (env, R.Choice [R.String (lhs ++ e) | e <- map R.toString rhs])
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)

evaluate env (Ast.Sub e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs - rhs))
        (R.Choice lhs, R.Choice rhs) -> (env, R.Choice [getResult (evaluate env (Sub l r)) | l <- map resToAst lhs, r <- map resToAst rhs ])
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [R.Int (e - rhs) | e <- R.extractIntFromCollection (R.Choice lhs)])
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [R.Int (lhs - e) | e <- R.extractIntFromCollection (R.Choice rhs)])
        (lhs, R.String s) -> (env, R.Fail)
        (R.String s, rhs) -> (env, R.Fail)
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)

evaluate env (Ast.Mul e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> (env, R.Int (lhs * rhs))
        (R.Choice lhs, R.Choice rhs) -> (env, R.Choice [getResult (evaluate env (Mul l r)) | l <- map resToAst lhs, r <- map resToAst rhs ])
        (R.Choice lhs, R.Int rhs) -> (env, R.Choice [R.Int (e * rhs) | e <- R.extractIntFromCollection (R.Choice lhs)])
        (R.Int lhs, R.Choice rhs) -> (env, R.Choice [R.Int (lhs * e) | e <- R.extractIntFromCollection (R.Choice rhs)])
        (lhs, R.String s) -> (env, R.Fail)
        (R.String s, rhs) -> (env, R.Fail)
        (lhs, R.Fail) -> (env, R.Fail)
        (R.Fail, rhs) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)

evaluate env (Ast.Lt e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs < rhs then (env, R.Int lhs) else (env, R.Fail)
        (R.String lhs, R.String rhs) -> if lhs < rhs then (env, R.String lhs) else (env, R.Fail)
        (R.Choice lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Lt l r)) | l <- map resToAst lhs, r <- map resToAst rhs])
        (R.Choice lhs, rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Lt l (resToAst rhs))) | l <- map resToAst lhs])           
        (lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Lt (resToAst lhs) r)) | r <- map resToAst rhs])
        (_, R.Fail) -> (env, R.Fail)
        (R.Fail, _) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)


evaluate env (Ast.Gt e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs > rhs then (env, R.Int lhs) else (env, R.Fail)
        (R.String lhs, R.String rhs) -> if lhs > rhs then (env, R.String lhs) else (env, R.Fail)
        (R.Choice lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Gt l r)) | l <- map resToAst lhs, r <- map resToAst rhs])
        (R.Choice lhs, rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Gt l (resToAst rhs))) | l <- map resToAst lhs])           
        (lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Gt (resToAst lhs) r)) | r <- map resToAst rhs])
        (_, R.Fail) -> (env, R.Fail)
        (R.Fail, _) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)

evaluate env (Ast.Eq e1 e2) = do
    lhs <- evaluate env e1
    rhs <- evaluate env e2
    case (lhs, rhs) of
        (R.Int lhs, R.Int rhs) -> if lhs == rhs then (env, R.Int lhs) else (env, R.Fail)
        (R.String lhs, R.String rhs) -> if lhs == rhs then (env, R.String lhs) else (env, R.Fail)
        (R.Choice lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Eq l r)) | l <- map resToAst lhs, r <- map resToAst rhs])
        (R.Choice lhs, rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Eq l (resToAst rhs))) | l <- map resToAst lhs])           
        (lhs, R.Choice rhs) -> evaluate env $ resToAst (R.Choice [getResult (evaluate env (Ast.Eq (resToAst lhs) r)) | r <- map resToAst rhs])
        (_, R.Fail) -> (env, R.Fail)
        (R.Fail, _) -> (env, R.Fail)
        (_, _) -> (env, R.Fail)

evaluate env (Ast.And e1 e2) = 
    case (e1, e2) of
        (e1, e2) -> do
            rhs <- evaluate env e1
            lhs <- evaluate env e2
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
            
evaluate env (Ast.ForDo domains range) = do
    let forEnv = allCombinations $ getEnv domains
            where
                getEnv [] = []
                getEnv ((Initialization var (Ast.Choice c)) : xs) = bindSingle var c : getEnv xs
                    where
                        bindSingle var (c:cs) = (var, c) : bindSingle var cs
                        bindSingle var c = []
                getEnv (x:xs) = getEnv xs
                allCombinations [] = [[]]
                allCombinations (x:xs) = [y:ys | y <- x, ys <- allCombinations xs] 
    let predicates = getPredicate domains
            where
                getPredicate [] = []
                getPredicate (Initialization var exp : xs) = getPredicate xs
                getPredicate (Declaration var exp : xs) = getPredicate xs
                getPredicate (x:xs) = x : getPredicate xs
    let domainEnv = do
            let evalEnv = [(fEnv, getResult $ evaluate (unionVars env (Map.fromList fEnv)) p) | fEnv <- (forEnv), p <- predicates]
            let validEnv = nub $ map fst $ concat $ filter (all (\(_, result) -> result /= R.Fail)) $ groupBy ((==) `on` getEnv) evalEnv
                        where
                            getEnv (e, _) = e
            validEnv
    let validNarrowEnv = do
            let domainEnv' = map construct $ groupBy ((==) `on` fst) $ nub $ sort $ concat domainEnv
                    where
                        construct [x] = x
                        construct (x:xs) = (fst x, Ast.Choice $ map snd (x:xs))
            let narrowEnvRes = narrowForEach narrowEnv (if null domainEnv' then env else (unionVars env (Map.fromList domainEnv')))
                    where 
                        narrowForEach [] e = []
                        narrowForEach (x:xs) e = do
                            let validValues = init $ narrowing (0, R.Nothing, range, e, fst x)
                                    where
                                        narrowing (n, R.Fail, _, _, _) = []
                                        narrowing (n, res, exp, e', varName) = do
                                             let result = getResult $ evaluate (insertVar e' (varName, IntLit n)) exp
                                             n : narrowing (n+1, result, exp, e', varName)
                            case validValues of
                                [one] -> (fst x, IntLit one) : narrowForEach xs (insertVar e (fst x, IntLit one))
                                multiple -> (fst x, Ast.Choice [IntLit value | value <- multiple]) : narrowForEach xs (insertVar e (fst x, Ast.Choice [IntLit value | value <- multiple]))
                        narrowEnv = getNarrowValues domains
                            where
                                getNarrowValues [] = []
                                getNarrowValues (Declaration var T.Int : xs) = (var, Ast.Type T.Int) : getNarrowValues xs
                                getNarrowValues (x:xs) = getNarrowValues xs
            let combinedEnv = allCombinations $ getCollection narrowEnvRes
                    where
                        getCollection [] = []
                        getCollection (x:xs) = inner x : getCollection xs
                            where
                                inner (var, Ast.Choice c) = zip (repeat var) c
                                inner (var, c) = [(var, c)]
                        allCombinations [] = [[]]
                        allCombinations (x:xs) = [y:ys | y <- x, ys <- allCombinations xs] 
            case predicates of
                [] -> narrowEnvRes
                predicate -> do
                    let evalEnv = [(cEnv, getResult $ evaluate (unionVars env (Map.fromList cEnv)) p) | cEnv <- combinedEnv, p <- predicates]
                    let validEnv = nub $ map fst $ concat $ filter (all (\(_, result) -> result /= R.Fail)) $ groupBy ((==) `on` getEnv) evalEnv
                            where
                                getEnv (e, _) = e
                    map construct $ groupBy ((==) `on` fst) $ nub $ sort $ concat validEnv
                                where
                                    construct [x] = x
                                    construct (x:xs) = (fst x, Ast.Choice $ map snd (x:xs))
    case (domainEnv, validNarrowEnv) of
        ([], []) ->         evaluate env $ BasicFor $ Ast.Choice $ map (resToAst . getResult . (\e -> evaluate ((unionVars env (Map.fromList e))) range)) forEnv
        ([], narrow) ->     evaluate env $ BasicFor $ Ast.Choice $ map (resToAst . getResult . (\e -> evaluate (insertVars (unionVars env (Map.fromList e)) narrow) range)) forEnv
        (domain, []) ->     evaluate env $ BasicFor $ Ast.Choice $ map (resToAst . getResult . (\e -> evaluate (unionVars env (Map.fromList e)) range)) domain
        (domain, narrow) -> evaluate env $ BasicFor $ Ast.Choice $ map (resToAst . getResult . (\e -> evaluate (insertVars (unionVars env (Map.fromList e)) narrow) range)) domain

evaluate env (Ast.FuncDef name params body return) = (insertFunc env (name, (params, body, return)), R.Nothing)

evaluate env (Ast.FuncCall name args) =
    case getFuncMetadata env name of
        Right metadata 
            | length (extractParams metadata) < length args -> error "Too much arguments provided" 
            | length (extractParams metadata) > length args -> error "Too few arguments provided" 
            -- | not $ isValidFuncArgType (extractParams metadata) $ map (getResult . evaluate env) args -> error "Invalid Type"
            | otherwise -> do
                let funcs = Map.fromList $ zipFuncs (extractParams metadata) args 
                        where
                            zipFuncs [] [] = []
                            zipFuncs ((pName, T.Func):params) (Ast.Var fName:args) = do
                                case getFuncMetadata env fName of
                                    Right fData -> (pName, fData) : zipFuncs params args
                                    Left msg -> zipFuncs params args
                            zipFuncs ((_, _):params) (_:args) = zipFuncs params args
                let vars = Map.fromList $ zipVars (extractParams metadata) args
                        where
                            zipVars [] [] = []
                            zipVars ((pName, T.Func):params) (arg:args) = zipVars params args
                            zipVars ((pName, pType):params) (arg:args) = 
                                case evaluate env (Ast.Unify (Ast.Type pType) ((resToAst . getResult . evaluate env) arg)) of
                                    (_, R.Fail) -> (pName, Ast.Fail) : zipVars params args
                                    (_, res) -> (pName, resToAst res) : zipVars params args
                let funcEnv = unionVars (unionFuncs env funcs) vars
                let (newFuncEnv, funcRes) = evaluate funcEnv (extractBody metadata)
                let boundedEnv = Map.fromList $ bind args (extractParams metadata) 
                        where 
                            bind [] [] = []
                            bind (Ast.Var var:as) ((pName, T.Func):ps) = bind as ps
                            bind (Ast.Var var:as) (p:ps) =
                                case getVarExp env var of
                                    Right exp -> 
                                        case getResult $ evaluate env (Ast.Unify exp (Ast.Type $ snd p)) of
                                            R.Fail -> bind as ps
                                            success -> 
                                                case getVarExp newFuncEnv (fst p) of 
                                                    Right exp' -> (var, exp') : bind as ps
                                                    Left msg -> bind as ps
                            bind (a:as) (p:ps) = bind as ps 
                (unionVars env boundedEnv, funcRes)                    
        Left msg -> error msg

evaluate env (Ast.Unify lhs rhs) = do
    case (lhs, rhs) of
        (Ast.Fail, rhs) -> (env, R.Fail)
        (lhs, Ast.Fail) -> (env, R.Fail)
        (Declaration var T.Int, rhs) -> do
            let (env', res') = evaluate env rhs
            case res' of
                R.Int n -> (insertVar env (var, rhs), R.Int n)
                R.Type T.Int -> (env, R.Type T.Int)
                _ -> (env, R.Fail)
        (lhs, Declaration var T.Int) -> evaluate env $ Ast.Unify (Declaration var T.Int) lhs 
        (Ast.Var var, e) -> do
            let (env', res') = evaluate env (Ast.Var var)
            let (env'', res'') = evaluate env e
            case (res', res'') of
                (R.Type T.Int, R.Int n) -> (insertVar env (var, e), R.Int n) 
                (R.Type (T.TupleFixed t1), R.Tuple t2) -> do
                    let results = map getResult $ zipWith (\l r -> evaluate env (Ast.Unify l r)) (map Ast.Type t1) (getCollection rhs)
                    case getResult $ evaluate env $ Ast.Tuple $ map resToAst results of
                        R.Fail -> (env, R.Fail)
                        success -> (insertVar (head $ reverse $ map (getEnvironment . (evaluate env)) (getCollection rhs)) (var, e), R.Nothing)
                (R.Tuple t1, R.Tuple t2)
                    | length t1 /= length t2 -> (env, R.Fail)
                    | otherwise -> do
                        case getVarExp env var of
                            Right ast ->
                                case (ast, rhs) of
                                    (Ast.Tuple c1, Ast.Tuple c2) -> do
                                        let vars = executePartialValues (zip c1 c2)
                                                where 
                                                    executePartialValues [] = []
                                                    executePartialValues ((Declaration var t, res):xs) = (var, res) : executePartialValues xs
                                                    executePartialValues ((res, Declaration var t):xs) = (var, res) : executePartialValues xs
                                                    executePartialValues (x:xs) = executePartialValues xs
                                        let newEnv = insertVars env vars
                                        let newTupleForVar = Ast.Tuple (assembleTuple c1)
                                                where 
                                                    assembleTuple [] = []
                                                    assembleTuple (Declaration var t:xs) = do 
                                                        case getVarExp newEnv var of
                                                            Right res -> res : assembleTuple xs
                                                            Left msg -> Declaration var t : assembleTuple xs 
                                                    assembleTuple (x:xs) = x : assembleTuple xs
                                        let newEnv' = insertVar newEnv (var, newTupleForVar)
                                        (newEnv', getResult (evaluate newEnv' newTupleForVar))                                
                            Left msg -> (env, R.Fail)
                (e1, e2) -> evaluate env (Ast.Unify (resToAst e1) (resToAst e2))
        (e, Ast.Var var) -> evaluate env $ Ast.Unify (Ast.Var var) e
        (Ast.Tuple t1, Ast.Tuple t2)
            | length t1 /= length t2 -> (env, R.Fail)
            | otherwise -> do
                let res = zipArgs t1 t2
                        where
                            zipArgs [] [] = []
                            zipArgs ((Declaration i ttype):xs) (y:ys) = 
                                case evaluate env (Ast.Unify (Ast.Type ttype) y) of
                                    (_, R.Fail) -> (env, R.Fail) : zipArgs xs ys
                                    (_, res) -> (insertVar env (i, y), res) : zipArgs xs ys
                            zipArgs (x:xs) ((Declaration i ttype):ys) = 
                                case evaluate env (Ast.Unify (Ast.Type ttype) x) of
                                    (_, R.Fail) -> (env, R.Fail) : zipArgs xs ys
                                    (_, res) -> (insertVar env (i, x), res) : zipArgs xs ys
                            zipArgs (x:xs) (y:ys) = evaluate env (Ast.Unify x y) : zipArgs xs ys
                case any ((== R.Fail) . getResult) res of
                    True -> (env, R.Fail)
                    False -> (mergeEnv (map getEnvironment res), R.Tuple (map getResult res))
        (e1, e2) -> do
            l <- evaluate env e1
            r <- evaluate env e2
            case (l, r) of 
                (R.Type T.Int, R.Int n) -> (env, R.Int n)
                (R.Int n, R.Type T.Int) -> (env, R.Int n)
                (R.Type T.String, R.String s) -> (env, R.String s)
                (R.String s, R.Type T.String) -> (env, R.String s)
                (R.Tuple t, R.Type T.Tuple) -> (env, R.Tuple t)
                (R.Type T.Tuple, R.Tuple t) -> (env, R.Tuple t)
                (R.Type (T.CustomType tDesc), R.CustomType tName fields) ->
                    case (getTypeName env tDesc, getTypeName env tName) of
                        (Right t1, Right t2)
                            | t1 == t2 -> (env, R.CustomType tName fields)
                            | otherwise -> (env, R.Fail)
                        (_, _) -> (env, R.Fail)
                (R.CustomType tName fields, R.Type (T.CustomType tDesc)) 
                    -> evaluate env (Ast.Unify (resToAst $ R.Type (T.CustomType tDesc)) (resToAst $ R.CustomType tName fields) )
                (R.Tuple t, R.Type (T.TupleFixed tt))
                    | length t /= length tt -> (env, R.Fail)
                    | otherwise -> do
                        let res = map getResult $ zipArgs (map resToAst t) tt
                                where 
                                    zipArgs [] [] = []
                                    zipArgs (x:xs) (y:ys) = evaluate env (Ast.Unify x (Ast.Type y)) : zipArgs xs ys
                        case any (== R.Fail) res of
                            True -> (env, R.Fail)
                            False -> (env, R.Tuple t)
                (R.Type (T.TupleFixed tt), R.Tuple t) -> evaluate env (Ast.Unify (Ast.Tuple (map resToAst t)) (Ast.Type (T.TupleFixed tt)))
                (R.Choice c, R.Type resType) -> 
                    case filter (/= R.Fail) $ map (getResult . (\x -> evaluate env (Ast.Unify (resToAst x) (Ast.Type resType)))) c of
                        [] -> (env, R.Fail)
                        res -> (env, R.Choice res)
                (R.Type resType, R.Choice c) -> 
                    case filter (/= R.Fail) $ map (getResult . (\x -> evaluate env (Ast.Unify (resToAst x) (Ast.Type resType)))) c of
                        [] -> (env, R.Fail)
                        res -> (env, R.Choice res)
                (a, R.Type T.Any) -> (env, a)
                (R.Type T.Any, a) -> (env, a)
                (R.Tuple t1, R.Tuple t2)
                    | length t1 /= length t2 -> (env, R.Fail)
                    | otherwise -> do
                        let res = zipArgs (map resToAst t1) (map resToAst t2)
                                where
                                    zipArgs [] [] = []
                                    zipArgs (x:xs) (y:ys) = evaluate env (Ast.Unify x y) : zipArgs xs ys
                        case any ((== R.Fail) . getResult) res of
                            True -> (env, R.Fail)
                            False -> (env, R.Tuple t1)
                (R.Int n1, R.Int n2)
                    | n1 == n2 -> (env, R.Int n1)
                    | otherwise -> (env, R.Fail)
                (R.String s1, R.String s2)
                    | s1 == s2 -> (env, R.String s1)
                    | otherwise -> (env, R.Fail)
                (R.Type t1, R.Type t2)
                    | t1 == t2 -> (env, R.Type t1)
                    | otherwise -> (env, R.Fail)
                (lhs, rhs)
                    | lhs == rhs -> (env, lhs)
                    | otherwise -> (env, R.Fail)