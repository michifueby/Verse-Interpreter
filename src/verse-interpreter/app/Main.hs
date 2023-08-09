{-
    Project:    Verse-Interpreter
    File:       Main.hs
-}

module Main (main) where
    
import System.IO ()

import qualified Data.Map as Map  

import Util.InputOutput.InputHandler 
import Util.Lexical.Lexer 
import Util.Parse.Parser
import Util.Evaluate.Evaluator 
import Util.Evaluate.EvaluatorAdditionals

-- import Util.Datatypes.Types as T
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Interface.ResultValue as R
        

{-
    Represents the entry point of the application.
-}
main :: IO ()
main = do 
    {- 
        --------------------------------
        |          Executable          |
        --------------------------------
    -}
    collection <- getContent "../samples/math.verse" 

    print (lexer collection)

    let env = Env {variables = Map.empty, functions = Map.empty}
    let ast = parse (filterUnusedRows (toRowNumber (lexer collection))) 

    --print(parse (filterUnusedRows (toRowNumber (lexer collection))) )

    print (foldl (evaluate . getEnvironment ) (env, R.Nothing) ast)

     {- 
        --------------------------------
        |          Examples            |
        --------------------------------
    -}
    
    -- Strings with Concat ++ operator     -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.ConcatString (StringLit "hallo") (StringLit "Welt"))) 

    -- Strings with <   -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.Lt (StringLit "hallo") (StringLit "Welt"))) 

    -- Strings with >    -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.Gt (StringLit "hallo") (StringLit "Welt"))) 

    -- Strings with >    -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.Eq (StringLit "hallo") (StringLit "Welt"))) 

    -- Strings with ==    -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.Eq (StringLit "hallo") (StringLit "hallo"))) 

    -- Strings with ==   -- =====Add for LP4 Logic Programmin Michael Füby
    print (evaluate env (Ast.Eq (StringLit "hallo") (StringLit "welt"))) 

    --Beispiel befindet sich auch im math.verse zum ausführen
    -- data Rectangle = Rectangle {width : int, height : int}

    -- Alles kann mit stack exec verse-interpreter-exe ausgeführt werden









    {- 
        ---------------------------------------------------
        |          Evaluator Testing Environment          |
        ---------------------------------------------------
    -}

    -- | Integer #1
    -- 2
    -- ==> 2
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (IntLit 2))  

    -- | Fail #1
    -- Fail
    -- ==> Fail
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Fail))  

    -- | Tuples #1
    -- (2,(10,20),6)
    -- ==> (2,(10,20),6)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [IntLit 2, Ast.Tuple [IntLit 10, IntLit 20], IntLit 6]))  

    -- | Tuples #2
    -- (2,3,6)
    -- ==> (2,3,6)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [IntLit 2, Ast.Tuple [IntLit 10, IntLit 20], IntLit 6]))  

    -- | Choice #1 
    -- (1|2|3)
    -- ==> (1|2|3)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Choice [IntLit 1, IntLit 2, IntLit 3])) 

    -- | Choice #2 
    -- (1..5)
    -- ==> (1|2|3|4|5)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Choice (map IntLit [1..5]))) 

    -- | Nested Choices #1 
    -- ((1|2), (7|8)) 
    -- ==> ((1,7), (1,8), (2,7), (2,8))
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [Ast.Choice [IntLit 1, IntLit 2], Ast.Choice [IntLit 7, IntLit 8]]))
    
    -- | Nested Choices #2 
    -- ((1|2), (7|8), 3) 
    -- ==> ((1,7,3), (1,8,3), (2,7,3), (2,8,3))
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [Ast.Choice [IntLit 1, IntLit 2], Ast.Choice [IntLit 7, IntLit 8], IntLit 3]))
    
    -- | Nested Choices #3 
    -- ((1|2), (7|8), (20,30)) 
    -- ==> ((1,7,(20,30)), (1,8,(20,30)), (2,7,(20,30)), (2,8,(20,30)))
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [Ast.Choice [IntLit 1, IntLit 2], Ast.Choice [IntLit 7, IntLit 8], Ast.Tuple [IntLit 20, IntLit 30]]))

    -- | Index Access Method #1
    -- (5,2,7,1)[2]
    -- ==> 7
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (IAM (Ast.Tuple [IntLit 5, IntLit 2, IntLit 7, IntLit 1]) (IntLit 2))) 

    -- | Index Access Method #2
    -- (5,2,7,1)[2 - 1]
    -- ==> 2
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (IAM (Ast.Tuple [IntLit 5, IntLit 2, IntLit 7, IntLit 1]) (Sub (IntLit 2) (IntLit 1)))) 
    
    -- | Index Access Method #3
    -- (5,2,7,1)[(0|1|0)]
    -- ==> 5|2|5
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (IAM (Ast.Tuple [IntLit 5, IntLit 2, IntLit 7, IntLit 1]) (Ast.Choice [IntLit 0, IntLit 1, IntLit 0])))
    
    -- | Index Access Method #4
    -- x <- 3
    -- (5,2,7,1)[x]
    -- ==> 1
    -- >>>>> let environment = Env {variables = Map.fromList [("x", IntLit 3)], functions = Map.empty}
    -- >>>>> print (evaluate environment (IAM (Ast.Tuple [IntLit 5, IntLit 2, IntLit 7, IntLit 1]) (Ast.Var "x")))

    -- | Var #1 
    -- x
    -- ==> x <- 2
    -- >>>>> let environment = Env {variables = Map.fromList [("x", IntLit 2)], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Var "x")) 

    -- | Var #2 
    -- x
    -- ==> x <- (1|2|3)
    -- >>>>> let environment = Env {variables = Map.fromList [("x", Ast.Choice [IntLit 1, IntLit 2, IntLit 3])], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Var "x")) 

    -- | Var #3 
    -- x
    -- ==> x <- (1,2,3)
    -- >>>>> let environment = Env {variables = Map.fromList [("x", Ast.Tuple [IntLit 1, IntLit 2, IntLit 3])], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Var "x")) 

    -- | Var #4 
    -- x
    -- ==> x <- 2 * 4
    -- >>>>> let environment = Env {variables = Map.fromList [("x", Ast.Mul (IntLit 2) (IntLit 4))], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Var "x")) 

    -- | Declaration #1 
    -- x:int
    -- ==> x <:- int
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Declaration "x" T.Int)) 

    -- | Declaration #2
    -- x:tuple(int,int)
    -- ==> x <:- tuple(int, int)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Declaration "x" (T.TupleFixed [T.Int, T.Int]))) 

    -- | Initialization #1
    -- x:=2
    -- ==> x <- 2 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Initialization "x" (IntLit 2))) 

    -- | Initialization #2
    -- x:= 2 + 3
    -- ==> x <- 2 + 3 !Lazy evaluation! 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Initialization "x" (Add (IntLit 2) (IntLit 3)))) 

    -- | Add #1
    -- 2 + 3
    -- ==> 5
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Add (IntLit 2) (IntLit 3)))  

    -- | Add #2
    -- 2 + false?
    -- ==> false?
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Add (IntLit 2) (Ast.Fail)))  

    -- | Add #3
    -- 2 + 3 + 7 + 8
    -- ==> 5
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Add (Add (IntLit 2) (IntLit 3)) (Add (IntLit 7) (IntLit 8))))  

    -- | Subtract #1
    -- 5 - 3
    -- ==> 2
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Sub (IntLit 5) (IntLit 3)))  

    -- | Subtract #2
    -- x <- 2
    -- 5 - x
    -- ==> 3
    -- >>>>> let environment = Env {variables = Map.fromList [("x", IntLit 2)], functions = Map.empty}
    -- >>>>> print (evaluate environment (Sub (IntLit 5) (Ast.Var "x")))  

    -- | Multiply #1
    -- 2 * 10
    -- ==> 20
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Mul (IntLit 2) (IntLit 10)))    

    -- | Multiply #2
    -- 2 * (1|3|5)
    -- ==> (2|6|10)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Mul (IntLit 2) (Ast.Choice [IntLit 1, IntLit 3, IntLit 5])))

    -- | Multiply #3
    -- x <- 2
    -- y <- 10
    -- x * y
    -- ==> 20
    -- >>>>> let environment = Env {variables = Map.fromList [("x", IntLit 2), ("y", IntLit 10)], functions = Map.empty}
    -- >>>>> print (evaluate environment (Mul (Ast.Var "x") (Ast.Var "y")))

    -- | LessThan #1
    -- 1 < 2
    -- ==> 1
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Lt (IntLit 1) (IntLit 2)))

    -- | LessThan #2
    -- 2 < 1
    -- ==> false?
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Lt (IntLit 2) (IntLit 1)))

    -- | GreaterThan #1
    -- 1 > 2
    -- ==> false?
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Lt (IntLit 1) (IntLit 2)))

    -- | GreaterThan #2
    -- 2 > 1
    -- ==> 2
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Lt (IntLit 2) (IntLit 1)))

    -- | Equals #1
    -- 1 == 1
    -- ==> 1
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Eq (IntLit 1) (IntLit 1)))

    -- | Equals #2
    -- 2 == 1
    -- ==> false?
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Eq (IntLit 2) (IntLit 1)))

    -- | Question ? #1
    -- (1,2,3)?
    -- ==> (1|2|3)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Question (Ast.Tuple [IntLit 1, IntLit 2, IntLit 3])))

    -- | Question ? #2
    -- (10, 20, (1,2,3)?)
    -- ==> (10, 20, (1|2|3)) >>> Should be: ((10,20,1),(10,20,2),(10,20,3))  
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.Tuple [IntLit 10, IntLit 20, Question (Ast.Tuple [IntLit 1, IntLit 2, IntLit 3])] ))
    
    -- | If-Statement #1
    -- if (1 + 2) then 3 else 2
    -- ==> 3  
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (Add (IntLit 1) (IntLit 2)) (IntLit 3) (IntLit 2)))

    -- | If-Statement #2
    -- if (1 > 2) then 3 else 2
    -- ==> 2 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (Gt (IntLit 1) (IntLit 2)) (IntLit 3) (IntLit 2)))

    -- | If-Statement #3
    -- if (1 < 2, 3 > 4) then 3 else 2
    -- ==> 2 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (And (Lt (IntLit 1) (IntLit 2)) (Gt (IntLit 3) (IntLit 4))) (IntLit 3) (IntLit 2)))
    
    -- | If-Statement #4
    -- if (1 < 2 | 3 > 4) then 3 else 2
    -- ==> 3 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (Or (Lt (IntLit 1) (IntLit 2)) (Gt (IntLit 3) (IntLit 4))) (IntLit 3) (IntLit 2)))
    
    -- | If-Statement #5
    -- if (1 < 2 | 3 > 4, 2 + 3) then 3 else 2
    -- ==> 3 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (Or (Lt (IntLit 1) (IntLit 2)) (And (Gt (IntLit 3) (IntLit 4)) (Add (IntLit 2) (IntLit 3)))) (IntLit 3) (IntLit 2)))
    
    -- | If-Statement #6
    -- x <- 3
    -- if (x = (2|3)) then 3 else 2
    -- ==> Error 
    -- >>>>> let environment = Env {variables = Map.fromList [("x", IntLit 3)], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.If (Eq (Ast.Var "x") (Ast.Choice [IntLit 2, IntLit 3])) (IntLit 3) (IntLit 2)))
    
    -- | For-Statement #1
    -- for (1|2|3) 
    -- ==> (1,2,3) 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.BasicFor (Ast.Choice [IntLit 1, IntLit 2, IntLit 3])))

    -- | For-Statement #2
    -- for (Fail) 
    -- ==> () 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.BasicFor (Ast.Fail)))

    -- | For-Statement #3
    -- for (i:=1..3) do (i*i)
    -- ==> (1,4,9) 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.ForDo (Initialization "i" (Ast.Choice [IntLit 1, IntLit 2, IntLit 3])) (Mul (Ast.Var "i") (Ast.Var "i"))))
    
    -- | For-Statement #4
    -- for (i:=1..3) do (i | i + 7)
    -- ==> ((1|8),(2|9),(3|10)) ==> Sould be Nested Choice
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.ForDo (Initialization "i" (Ast.Choice [IntLit 1, IntLit 2, IntLit 3])) (Ast.Choice [Ast.Var "i", Add (Ast.Var "i") (IntLit 7)])))

    -- | For-Statement #5
    -- for (i:=1..4) do (i < 3)
    -- ==> (1, 2, false?, false?) ==> Should be false? 
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.ForDo (Initialization "i" (Ast.Choice [IntLit 1, IntLit 2, IntLit 3, IntLit 4])) (Lt (Ast.Var "i") (IntLit 3))))
    
    -- | For-Statement #5
    -- for (i:=1..4, i<3) do (i * i)
    -- ==> Not working ==> (i:=1..4, i<3) works
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.ForDo (And (Initialization "i" (Ast.Choice [IntLit 1, IntLit 2, IntLit 3, IntLit 4])) (Lt (Ast.Var "i") (IntLit 3))) (Mul (Ast.Var "i") (Ast.Var "i")) ))
    
    -- | For-Statement #6
    -- as := (3,7,4)
    -- for (i:int) do (as[i] + 1)
    -- ==> (4,8,5); i<-(0|1|2)
    -- >>>>> let environment = Env {variables = Map.fromList [("as", Ast.Tuple [IntLit 3, IntLit 7, IntLit 4])], functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.ForDo (Declaration "i" T.Int) (Add (IAM (Ast.Var "as") (Ast.Var "i")) (IntLit 1))))

    -- | Functions #1
    -- add(x:int, y:int):int := x+y
    -- ==> add <- (x:int,y:int) (x + y) (int)
    -- >>>>> let environment = Env {variables = Map.empty, functions = Map.empty}
    -- >>>>> print (evaluate environment (Ast.FuncDef "add" [("x", T.Int), ("y", T.Int)] (Add (Ast.Var "x") (Ast.Var "y")) T.Int ))
    
    -- | Functions #2
    -- add (3,4)
    -- ==> 7
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.empty, 
    -- >>>>>     functions = Map.fromList [("add", ([("x", T.Int),("y", T.Int)],Add (Ast.Var "x") (Ast.Var "y"),T.Int))]
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Ast.FuncCall "add" [IntLit 3, IntLit 4]))

    -- | Functions #3
    -- fac (5)
    -- ==> 120
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.empty, 
    -- >>>>>     functions = Map.fromList [("fac", ([("x", T.Int)], (If (Eq (Ast.Var "x") (IntLit 0)) (IntLit 1) (Mul (Ast.Var "x") (FuncCall "fac" ([Sub (Ast.Var "x") (IntLit 1)])))), T.Int))]
    -- >>>>>     }
    -- >>>>> print (evaluate environment (FuncCall "fac" [IntLit 5]))

    -- | Unification #1
    -- x = 3
    -- ==> 3; x <- 3
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.fromList [("x", Ast.Type T.Int)], 
    -- >>>>>     functions = Map.empty
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Unify (Ast.Var "x") (IntLit 3)))
    
    -- | Unification #2
    -- 2 = x
    -- ==> 2; x <- 2
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.fromList [("x", Ast.Type T.Int)], 
    -- >>>>>     functions = Map.empty
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Unify (IntLit 2) (Ast.Var "x")))
    
    -- | Unification #3
    -- 2 = 3
    -- ==> Fail
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.empty, 
    -- >>>>>     functions = Map.empty
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Unify (IntLit 2) (IntLit 3)))
    
    -- | Unification #4
    -- 2 = 2
    -- ==> 2
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.empty, 
    -- >>>>>     functions = Map.empty
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Unify (IntLit 2) (IntLit 2)))
    
    -- | Unification #5
    -- 2 + 2 = 2 + 3
    -- ==> Fail
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.empty, 
    -- >>>>>     functions = Map.empty
    -- >>>>>     }
    -- >>>>> print (evaluate environment (Unify (Add (IntLit 2) (IntLit 2)) (Add (IntLit 2) (IntLit 3))))

    -- | Partial Values #1
    -- x : tuple (int, int)
    -- x : (2, y:int)
    -- x : (z:int, 3)
    -- ==> x <- (2,3)
    -- >>>>> let environment = Env { 
    -- >>>>>     variables = Map.fromList [("x", Ast.Type (T.TupleFixed [T.Int, T.Int]))],
    -- >>>>>     functions = Map.empty
    -- >>>>> }
    -- >>>>> print (foldl (evaluate . getEnvironment) (environment, R.Int 0) [Unify (Ast.Var "x") (Ast.Tuple [IntLit 2, Declaration "y" T.Int]), Unify (Ast.Var "x") (Ast.Tuple [Declaration "z" T.Int, IntLit 3])])


