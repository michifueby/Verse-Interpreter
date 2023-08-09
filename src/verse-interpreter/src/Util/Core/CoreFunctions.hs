{-
    Project:    Verse-Interpreter
    File:       CoreFunctions.hs
-}

module Util.Core.CoreFunctions
    ( getUserDefinedFunctions
    )
where

import qualified Data.Map as Map
import qualified Util.Datatypes.Types as T
import Util.Interface.AbstractSyntaxTree as Ast

import Util.Evaluate.Environment
import Util.Shared


{- 
    -------------------------------------------
    |             Core Functions              |
    -------------------------------------------
-}

{-
    Gets the User Defined Functions.
-}
getUserDefinedFunctions :: Env
getUserDefinedFunctions = Env {
                variables = Map.empty,
                functions = Map.fromList [
                    ("head", ([("xs", T.Tuple)], Ast.IndexAccess (Ast.Var "xs") (Ast.IntLit 0), T.Int)),
                    ("tail", ([("xs", T.Tuple)], Ast.ForDo [Ast.Declaration "i" T.Int , Ast.Gt (Ast.Var "i") (Ast.IntLit 0)] (Ast.IndexAccess (Ast.Var "xs") (Ast.Var "i")), T.Int)),
                    ("append", ([("xs", T.Tuple), ("ys", T.Tuple)], BasicFor (Choice [IndexAccess (Var "xs") (Declaration "i" T.Int), IndexAccess (Var "ys") (Declaration "j" T.Int)]) , T.Int)),
                    ("map", ([("f", T.Func), ("xs", T.Tuple)], BasicFor (FuncCall "f" [IndexAccess (Var "xs") (Declaration "i" T.Int)]), T.Any)),
                    ("countElementsFrom", ([("i", T.Int), ("xs", T.Tuple)], Ast.If (Ast.IndexAccess (Ast.Var "xs") (Ast.Var "i")) (Ast.FuncCall "countElementsFrom" [Ast.Add (Ast.Var "i") (Ast.IntLit 1), Ast.Var "xs"]) (Ast.Var "i"), T.Int)),
                    ("length", ([("xs", T.Tuple)], Ast.FuncCall "countElementsFrom" [Ast.IntLit 0, Ast.Var "xs"], T.Int)),
                    ("abs", ([("x", T.Int)], Ast.If (Ast.Lt (Ast.Var "x") (Ast.IntLit 0)) (Ast.Mul (Ast.Var "x") (Ast.IntLit (-1))) (Ast.Var "x"), T.Int)),
                    ("isEven", ([("x", T.Int)], 
                        Ast.Seq One [
                            Ast.Initialization "absValue" (Ast.FuncCall "abs" [Ast.Var "x"]), 
                            Ast.If (Ast.Eq (Ast.Var "absValue") (Ast.IntLit 0))
                                (Ast.Var "absValue")
                                (Ast.If (Ast.Eq (Ast.Var "absValue") (Ast.IntLit 1)) 
                                    Ast.Fail 
                                    (Ast.FuncCall "isEven" [Ast.Sub (Ast.Var "absValue") (Ast.IntLit 2)]))
                            ] , T.Int))
                ],
                types = Map.empty
            }