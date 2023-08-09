{
module Util.Parsing.Grammars.Grammar where
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Interface.Token.Token
import Util.Datatypes.Types as T
import Util.Shared
import qualified Data.Map as Map

}

%name parseExp
%tokentype { Token }
%error { parseError }

%token
    num     { TokenNumber $$ }
    name    { TokenName $$ }
    str     { TokenString $$ }
    '+'     { TokenPrimitives "+" }  
    '..'    { TokenPrimitives ".." }  
    '-'     { TokenPrimitives "-" }  
    '*'     { TokenPrimitives "*" }  
    '<'     { TokenPrimitives "<" }  
    '>'     { TokenPrimitives ">" }  
    '='     { TokenPrimitives "=" }  
    ':'     { TokenPrimitives ":" }  
    ','     { TokenPrimitives "," }
    '|'     { TokenPrimitives "|" }
    '?'     { TokenPrimitives "?" }  
    '.'     { TokenPrimitives "." }  
    '=>'    { TokenPrimitives "=>" }  
    ';'     { TokenSemicolon }  
    '\n'    { TokenNewLine }  
    '('     { TokenLeft Rounded }
    ')'     { TokenRight Rounded }
    '['     { TokenLeft Squared }  
    ']'     { TokenRight Squared  }  
    '}'     { TokenRight Curly  }  
    '{'     { TokenLeft Curly }  
    if      { TokenKey "if" } 
    then    { TokenKey "then" } 
    else    { TokenKey "else" } 
    for     { TokenKey "for" } 
    do      { TokenKey "do" } 
    data    { TokenKey "data" } 
    new     { TokenKey "new" } 
    Tstring { TokenKey "string" } 
    Tint    { TokenKey "int" } 
    Ttuple  { TokenKey "tuple" }
    Tfunc   { TokenKey "func" }
    Tany    { TokenKey "any" } 
    false   { TokenKey "false?" } 
    array   { TokenKey "array" } 

%%

{- 
    -------------------------------------------
    |            Expression Parser            |                
    -------------------------------------------
-}
parseExp :: { VerseExp }
    : sequential    { $1 }



{- 
    -------------------------------------------
    |           Expression Grammar            |                
    -------------------------------------------
-}
expression :: { VerseExp }
    : binOp                 { $1 }
    | initialization        { $1 }
    | declaration           { $1 }
    | typeInitialization    { $1 }
    | fieldInstance         { $1 }
    | fieldAccess           { $1 }
    | fieldInitialization   { $1 }
    | choice                { $1 }
    | tuple                 { $1 }
    | ifExp                 { $1 }
    | forExp                { $1 }
    | indexAccess           { $1 }
    | funcDef               { $1 }
    | funcCall              { $1 }
    
{- 
    -------------------------------------------
    |      Sequential Expressions Grammar     |                
    -------------------------------------------
-}
sequential :: { VerseExp }
    : expression                        { $1 }
    | expressions                       { Seq One ($1) }


expressions :: { [VerseExp] }
    : expression                    { [$1] }
    | '\n'                          { [] }
    | ';'                           { [] }
    | '\n' expressions              { $2 }
    | expression '\n'               { [$1] }
    | expression ';'                { [$1] }
    | expressions '\n' expressions  { $1 }
    | expression ';' expressions    { [$1] ++ $3}
    | expression '\n' expressions   { [$1] ++ $3}

{- 
    -------------------------------------------
    |              Function Grammar           |                
    -------------------------------------------
-}
funcDef :: { VerseExp }
    : name '(' params ')' ':' type ':' '=' body                 { FuncDef $1 $3 $9 $6 }
    | name ':' '=' '(' ')' ':' type ':' '=' body                { FuncDef $1 [] $10 $7}
    | name '(' params ')' ':' '=' body                          { FuncDef $1 $3 $7 T.Any }
    | name '(' ')' ':' '=' body                                 { FuncDef $1 [] $6 T.Any }
    | name ':' '=' '(' params '=>' body ')' ':' type            { FuncDef $1 $5 $7 $10 }
    | name ':' '=' '(' '(' params ')' '=>' body ')' ':' type    { FuncDef $1 $6 $9 $12 }
    | name ':' '=' '(' '(' ')' '=>' body ')' ':' type           { FuncDef $1 [] $8 $11 }
    | name ':' '=' '(' params '=>' body ')'                     { FuncDef $1 $5 $7 T.Any }
    | name ':' '=' '(' '(' params ')' '=>' body ')'             { FuncDef $1 $6 $9 T.Any }
    | name ':' '=' '(' '(' ')' '=>' body ')'                    { FuncDef $1 [] $8 T.Any }

params :: { Parameters }
    : name ':' Tint                 { [($1, T.Int)] }
    | name ':' Tint ',' params      { [($1, T.Int)] ++ $5 }
    | name ':' Ttuple               { [($1, T.Tuple)] }
    | name ':' Ttuple ',' params    { [($1, T.Tuple)] ++ $5 }
    | name ':' Tstring              { [($1, T.String)] }
    | name ':' Tstring ',' params   { [($1, T.String)] ++ $5 }
    | name ':' Tfunc                { [($1, T.Func)] }
    | name ':' Tfunc ',' params     { [($1, T.Func)] ++ $5 }
    | name ':' Tany                 { [($1, T.Any)] }
    | name ':' Tany ',' params      { [($1, T.Any)] ++ $5 }

body :: { VerseExp }
    : expression            { $1 }
    | '{' sequential '}'    { $2 }

funcCall :: { VerseExp }
    : name '(' args ')' { FuncCall $1 $3 }
    | name '(' name ')' { FuncCall $1 [(Ast.Var $3)] }
    | name '(' ')'      { FuncCall $1 [] }

args :: { [VerseExp] }
    : name                                  { [Ast.Var $1] }
    | name ',' args                         { [Ast.Var $1] ++ $3 }
    | num                                   { [IntLit $1] }
    | num ',' args                          { [IntLit $1] ++ $3 }
    | binOp                                 { [$1]}
    | binOp ',' args                        { [$1] ++ $3 }
    | tuple                                 { [$1]}
    | tuple ',' args                        { [$1] ++ $3 }
    | choice                                { [$1]}
    | choice ',' args                       { [$1] ++ $3 }
    | name '[' binOp ']'                    { [IndexAccess (Ast.Var $1) $3] }
    | name '[' binOp ']' ',' args           { [IndexAccess (Ast.Var $1) $3] ++$6 }
    | name '[' name ':' Tint ']'            { [IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] }
    | name '[' name ':' Tint ']' ',' args   { [IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] ++ $8 }


{- 
    -------------------------------------------
    |               For Grammar               |                
    -------------------------------------------
-}
forExp :: { VerseExp }
    : for '{' choice '}'                                { BasicFor $3 }
    | for '{' binOp '}'                                 { BasicFor $3 }
    | for '{' false '}'                                 { BasicFor Ast.Fail }
    | for '(' domain ')' do '(' range ')'               { ForDo $3 $7 }
    | for '(' domain ')' do '{' sequential '}'          { ForDo $3 $7 }
    | for '{' forScope '}'                              { ForDo (init $3) (last $3)}



domain :: { [VerseExp] }
    : initialization            { [$1] }
    | declaration               { [$1] }
    | binOp                     { [$1] }
    | binOp ',' domain          { ([$1] ++ $3) }
    | domain ',' binOp          { ($1 ++ [$3]) }
    | initialization ',' domain { ([$1] ++ $3) }
    | domain ',' initialization { ($1 ++ [$3]) }
    | declaration ',' domain    { ([$1] ++ $3) }
    | domain ',' declaration    { ($1 ++ [$3]) }

forScope :: { [VerseExp] }
    : initialization                { [$1] }
    | forScope ';' initialization   { $1 ++ [$3] }
    | initialization ';' forScope   { [$1] ++ $3 }
    | declaration                   { [$1] }
    | declaration ';' forScope      { [$1] ++ $3 }
    | forScope ';' declaration      { $1 ++ [$3] }
    | binOp                         { [$1] }
    | binOp ';' forScope            { [$1] ++ $3 }
    | forScope ';' binOp            { $1 ++ [$3] }
    | tuple                         { [$1] }
    | tuple ';' forScope            { [$1] ++ $3}
    | forScope ';' tuple            { $1 ++ [$3]}

range :: { VerseExp }
    : expression { $1 }

{- 
    -------------------------------------------
    |            Index Access Grammar         |                
    -------------------------------------------
-}
indexAccess :: { VerseExp }
    : name '[' binOp ']'            { IndexAccess (Ast.Var $1) $3 }
    | tuple '[' binOp ']'           { IndexAccess $1 $3 }
    | name '[' name ':' Tint ']'    { IndexAccess (Ast.Var $1) (Declaration $3 T.Int)}
    | tuple '[' name ':' Tint ']'   { IndexAccess $1 (Declaration $3 T.Int) }

{- 
    -------------------------------------------
    |               If Grammar                |                
    -------------------------------------------
-}

ifExp :: { VerseExp }
    : if '(' condition ')' then ifScope else ifScope    { If $3 $6 $8 }
    | if condition then ifScope else ifScope            { If $2 $4 $6 }

condition :: { VerseExp }
    : ifOp      { $1 }

ifOp :: { VerseExp }
    : ifOp '<' Exp2     { Ast.Lt $1 $3 }
    | ifOp '>' Exp2     { Ast.Gt $1 $3 }
    | ifOp '=' Exp2     { Ast.Eq $1 $3 }
    | Exp2              { $1 }

Exp2 :: { VerseExp }
    : Exp2 '+' term2    { Add $1 $3 }
    | Exp2 '-' term2    { Sub $1 $3 }
    | term2             { $1 }

term2 :: { VerseExp }
    : term2 '*' factor2     { Mul $1 $3 }
    | factor2               { $1 }

factor2 :: { VerseExp }
    : '(' ifOp ')'              { $2 }
    | '-' num                   { IntLit (-$2) }      
    | num                       { IntLit $1 }
    | name                      { Var $1 }
    | str                       { Ast.String $1 }
    | ifIndexAccess             { $1 }
    | ifFuncCall                { $1 }
    | ifChoice                  { $1 }
    | ifTuple                   { $1 }
    | ifFieldAccess             { $1 }

ifScope :: { VerseExp }
    : expression            { $1 }
    | '{' sequential '}'    { $2 }

ifIndexAccess :: { VerseExp }
    : name '[' ifOp ']'            { IndexAccess (Ast.Var $1) $3 }
    | ifTuple '[' ifOp ']'           { IndexAccess $1 $3 }
    | name '[' name ':' Tint ']'    { IndexAccess (Ast.Var $1) (Declaration $3 T.Int)}
    | ifTuple '[' name ':' Tint ']'   { IndexAccess $1 (Declaration $3 T.Int) }

ifFuncCall :: { VerseExp}
    : name '(' args ')' { FuncCall $1 $3 }
    | name '(' name ')' { FuncCall $1 [(Ast.Var $3)] }
    | name '(' ')'      { FuncCall $1 [] }

ifChoice :: { VerseExp }
    -- Int Lit 
    : num '|' ifChoices                 { Ast.Choice ([IntLit $1] ++ $3) }   
    | '(' num '|' ifChoices ')'         { Ast.Choice ([IntLit $2] ++ $4) }
    | num '..' num                      { Ast.Choice [IntLit x | x <- [$1..$3]] }
    -- Strings
    | str '|' ifChoices               { Ast.Choice ([Ast.String $1] ++ $3) }   
    | '(' str '|' ifChoices ')'       { Ast.Choice ([Ast.String $2] ++ $4) }
    -- -- Var Call
    | name '|' ifChoices              { Ast.Choice ([Ast.Var $1] ++ $3) }
    | '(' name '|' ifChoices ')'      { Ast.Choice ([Ast.Var $2] ++ $4) }
    -- -- Func Call
    | name '(' args ')' '|' ifChoices { Ast.Choice ([Ast.FuncCall $1 $3] ++ $6) }
    | name '(' ')' '|' ifChoices      { Ast.Choice ([Ast.FuncCall $1 []] ++ $5) }
    -- -- Index Access
    | name '[' name ':' Tint ']' '|' ifChoices   { Ast.Choice ([Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] ++ $8)}
    | name '[' ifOp ']' '|' ifChoices            { Ast.Choice ([Ast.IndexAccess (Ast.Var $1) $3] ++ $6) }
    | '(' name '[' ifOp ']' '|' ifChoices ')'    { Ast.Choice ([Ast.IndexAccess (Ast.Var $2) $4] ++ $7) }
    | ifTuple '[' ifOp ']' '|' ifChoices           { Ast.Choice ([Ast.IndexAccess $1 $3] ++ $6) }
    | '(' ifTuple '[' ifOp ']' '|' ifChoices ')'   { Ast.Choice ([Ast.IndexAccess $2 $4] ++ $7) }
    

ifChoices :: { [VerseExp] }
    -- Int Lit
    : num                                   { [IntLit $1] }
    | num '|' ifChoices                     { [IntLit $1] ++ $3 }
    -- Tuple
    | ifTuple                                 { [$1] }
    | ifTuple '|' ifChoices                   { [$1] ++ $3 }
    -- Strings
    | str                                   { [Ast.String $1] }
    | str '|' ifChoices                     { [Ast.String $1] ++ $3 }
    -- -- Var Calls
    | name                                  { [Ast.Var $1] }
    | name '|' ifChoices                    { [Ast.Var $1] ++ $3 }
    -- -- Func Calls
    | name '(' args ')'                     { [FuncCall $1 $3]}
    | name '(' args ')' '|' ifChoices       { [FuncCall $1 $3] ++ $6 } 
    | name '(' ')'                          { [FuncCall $1 []]}
    | name '(' ')' '|' ifChoices            { [FuncCall $1 []] ++ $5 }
    -- -- Index Access
    | name '[' name ':' Tint ']'               { [Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] }
    | name '[' name ':' Tint ']' '|' ifChoices { [Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] ++ $8 }
    | name '[' ifOp ']'                        { [Ast.IndexAccess (Ast.Var $1) $3] }
    | name '[' ifOp ']' '|' ifChoices          { [Ast.IndexAccess (Ast.Var $1) $3] ++ $6 }
    | ifTuple '[' ifOp ']'                       { [Ast.IndexAccess $1 $3] }
    | ifTuple '[' ifOp ']' '|' ifChoices         { [Ast.IndexAccess $1 $3] ++ $6 }

ifTuple :: { VerseExp }
    : '(' ')'                           { Ast.Tuple [] }
    | '(' ifTupleElements ')'         { Ast.Tuple $2 }
    | array '{' ifTupleElements '}'   { Ast.Tuple $3 }

ifTupleElements :: { [VerseExp] }
    : ifOp                              { [$1] }
    | ifOp ',' ifTupleElements          { [$1] ++ $3}
    | ifTuple                             { [$1] }
    | ifTuple ',' ifTupleElements         { [$1] ++ $3 }
    | name ':' Tint                     { [Declaration $1 T.Int] }
    | name ':' Tint ',' ifTupleElements { [Declaration $1 T.Int] ++ $5}


ifFieldAccess :: { VerseExp }
    : name '.' name     { FieldAccess $1 $3 }

{- 
    -------------------------------------------
    |               Tuple Grammar             |                
    -------------------------------------------
-}
tuple :: { VerseExp }
    : '(' ')'                       { Ast.Tuple [] }
    | '(' tupleElements ')'         { Ast.Tuple $2 }
    | array '{' tupleElements '}'   { Ast.Tuple $3 }

tupleElements :: { [VerseExp] }
    : binOp                             { [$1] }
    | binOp ',' tupleElements           { [$1] ++ $3}
    | tuple                             { [$1] }
    | tuple ',' tupleElements           { [$1] ++ $3 }
    | name ':' Tint                     { [Declaration $1 T.Int] }
    | name ':' Tint ',' tupleElements   { [Declaration $1 T.Int] ++ $5}

{- 
    -------------------------------------------
    |               Choice Grammar            |                
    -------------------------------------------
-}
choice :: { VerseExp }
    -- Int Lit 
    : num '|' choices               { Ast.Choice ([IntLit $1] ++ $3) }   
    | '(' num '|' choices ')'       { Ast.Choice ([IntLit $2] ++ $4) }
    | num '..' num                  { Ast.Choice [IntLit x | x <- [$1..$3]] }
    -- Strings
    | str '|' choices               { Ast.Choice ([Ast.String $1] ++ $3) }   
    | '(' str '|' choices ')'       { Ast.Choice ([Ast.String $2] ++ $4) }
    -- Var Call
    | name '|' choices              { Ast.Choice ([Ast.Var $1] ++ $3) }
    | '(' name '|' choices ')'      { Ast.Choice ([Ast.Var $2] ++ $4) }
    -- Func Call
    | name '(' args ')' '|' choices { Ast.Choice ([Ast.FuncCall $1 $3] ++ $6) }
    | name '(' ')' '|' choices      { Ast.Choice ([Ast.FuncCall $1 []] ++ $5) }
    -- Index Access
    | name '[' name ':' Tint ']' '|' choices    { Ast.Choice ([Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] ++ $8)}
    | name '[' binOp ']' '|' choices            { Ast.Choice ([Ast.IndexAccess (Ast.Var $1) $3] ++ $6) }
    | '(' name '[' binOp ']' '|' choices ')'    { Ast.Choice ([Ast.IndexAccess (Ast.Var $2) $4] ++ $7) }
    | tuple '[' binOp ']' '|' choices           { Ast.Choice ([Ast.IndexAccess $1 $3] ++ $6) }
    | '(' tuple '[' binOp ']' '|' choices ')'   { Ast.Choice ([Ast.IndexAccess $2 $4] ++ $7) }
    


choices :: { [VerseExp] }
    -- Int Lit
    : num                               { [IntLit $1] }
    | num '|' choices                   { [IntLit $1] ++ $3 }
    -- Strings
    | str                               { [Ast.String $1] }
    | str '|' choices                   { [Ast.String $1] ++ $3 }
    -- Var Calls
    | name                              { [Ast.Var $1] }
    | name '|' choices                  { [Ast.Var $1] ++ $3 }
    -- Func Calls
    | name '(' args ')'                 { [FuncCall $1 $3]}
    | name '(' args ')' '|' choices     { [FuncCall $1 $3] ++ $6 } 
    | name '(' ')'                      { [FuncCall $1 []]}
    | name '(' ')' '|' choices          { [FuncCall $1 []] ++ $5 }
    -- Index Access
    | name '[' name ':' Tint ']'                { [Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] }
    | name '[' name ':' Tint ']' '|' choices    { [Ast.IndexAccess (Ast.Var $1) (Declaration $3 T.Int)] ++ $8 }
    | name '[' binOp ']'                        { [Ast.IndexAccess (Ast.Var $1) $3] }
    | name '[' binOp ']' '|' choices            { [Ast.IndexAccess (Ast.Var $1) $3] ++ $6 }
    | tuple '[' binOp ']'                       { [Ast.IndexAccess $1 $3] }
    | tuple '[' binOp ']' '|' choices           { [Ast.IndexAccess $1 $3] ++ $6 }


{- 
    -------------------------------------------
    |           Field Instance Grammar        |                
    -------------------------------------------
-}

fieldInstance :: { VerseExp }
    : name ':' '=' new name                     { FieldInstance $1 (TypeData $5 (Map.empty)) }
    | name ':' '=' new name '{' fields '}'      { FieldInstance $1 (TypeData $5 (Map.fromList $7 ))}


{- 
    -------------------------------------------
    |      Field Initialization Grammar       |                
    -------------------------------------------
-}

fieldInitialization :: { VerseExp }
    : name '.' name ':' '=' binOp           { FieldInitialization $1 $3 $6 }
    | name '.' name ':' '=' tuple           { FieldInitialization $1 $3 $6 }

{- 
    -------------------------------------------
    |          Field Access Grammar           |                
    -------------------------------------------
-}

fieldAccess :: { VerseExp }
    : name '.' name     { FieldAccess $1 $3 }

{- 
    -------------------------------------------
    |    Custom type Initialization Grammar   |                
    -------------------------------------------
-}

typeInitialization :: { VerseExp }
    : data name ':' '=' '{' fields '}'  { TypeDef $2 (Map.fromList $6) }

fields :: { [(FieldName, Exp)] }
    : name ':' '=' binOp                { [($1, $4)] }
    | name ':' '=' binOp ',' fields     { [($1, $4)] ++ $6 }
    | name ':' '=' tuple                { [($1, $4)] }
    | name ':' '=' tuple ',' fields     { [($1, $4)] ++ $6 }
    | name ':' type                     { [($1, Ast.Type $3)] }
    | name ':' type ',' fields          { [($1, Ast.Type $3)] ++ $5 } 
    
{- 
    -------------------------------------------
    |           Initialization Grammar        |                
    -------------------------------------------
-}
initialization :: { VerseExp }
    : name ':' '=' binOp        { Initialization $1 $4 }
    | name ':' '=' choice       { Initialization $1 $4 }
    | name ':' '=' tuple        { Initialization $1 $4 }
    | name ':' '=' ifExp        { Initialization $1 $4 }
    | name ':' '=' forExp       { Initialization $1 $4 }

{- 
    -------------------------------------------
    |            Declaration Grammar          |                
    -------------------------------------------
-}
declaration :: { VerseExp }
    : name ':' type     { Declaration $1 $3 }

{- 
    -------------------------------------------
    |              Type Grammar               |                
    -------------------------------------------
-}

type :: { T.Type }
    : Tint                  { T.Int }
    | Tstring               { T.String }
    | Ttuple '(' types ')'  { T.TupleFixed $3 }
    | Ttuple                { T.Tuple }
    | Tfunc                 { T.Func }
    | name                  { T.CustomType $1 } 

types :: { [T.Type] }
    : type                  { [$1] }
    | type ',' types        { [$1] ++ $3 }

{- 
    -------------------------------------------
    |          Binary Operation Grammar       |                
    -------------------------------------------
-}
binOp :: { VerseExp }
    : logicOp   { $1 }

logicOp :: { VerseExp }
    : logicOp '<' Exp   { Ast.Lt $1 $3 }
    | logicOp '>' Exp   { Ast.Gt $1 $3 }
    | logicOp '=' Exp   { Ast.Unify $1 $3 }
    | Exp               { $1 }

Exp :: { VerseExp }
    : Exp '+' term    { Add $1 $3 }
    | Exp '-' term    { Sub $1 $3 }
    | term            { $1 }

term :: { VerseExp }
    : term '*' factor   { Mul $1 $3 }
    | factor            { $1 }

factor :: { VerseExp }
    : '(' binOp ')'             { $2 }
    | '(' '-' num ')'           { IntLit (-$3) } 
    | str                       { Ast.String $1 }
    | num                       { IntLit $1 }
    | name                      { Ast.Var $1 }
    | indexAccess               { $1 }
    | funcCall                  { $1 }
    | choice                    { $1 }
    | tuple                     { $1 }
    | fieldAccess               { $1 }
    | new name '{' fields '}'   { TypeData $2 (Map.fromList $4) }
    | false                     { Fail }
{
parseError :: [Token] -> a
parseError t = error $ "Parse error " ++ show t
}