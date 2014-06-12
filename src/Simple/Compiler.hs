module Simple.Compiler where

import           Simple.AST as AST
import           Simple.VM  as VM

class Compilable a where
  compile :: a -> Bytecode

instance Compilable Stmt where
  compile (Seq stmts)                 = concatMap compile stmts
  compile (AST.While cond stmts)      = [Const $ Code (compile stmts)]
                                     ++ [Const $ Code (compile cond)]
                                     ++ [VM.While]
  compile (IfElse cond stmts1 stmts2) = [Const $ Code (compile stmts1)]
                                     ++ [Const $ Code (compile stmts2)]
                                     ++ compile cond
                                     ++ [VM.If]
  compile (AST.If cond stmts)         = compile $ IfElse cond stmts (Seq [])
  compile (Init varType var expr)     = compile $ Set var expr
  compile (Expr expr)                 = compile expr ++ [ClearStack]

instance Compilable Expr where
  compile (Set var expr)              = compile expr ++ [Store var]
  compile (FuncCall func args)        = if func == "print"
    then concatMap compile args ++ [Print]
    else error $ "Unknown function " ++ func
  compile (Var var)                   = [Load var]
  compile (IntLit int)                = [Const $ I int]
  compile (BoolLit bool)              = [Const $ B bool]
  compile (UnaryOp PreDec (Var var))  =
    [ Load var
    , Const (I 1)
    , VM.Sub
    , Store var
    , Load var
    ]
  compile (UnaryOp PreDec _)          = incrementVarError True True
  compile (UnaryOp PostDec (Var var)) =
    [ Load var
    , Load var
    , Const (I 1)
    , VM.Sub
    , Store var
    ]
  compile (UnaryOp PostDec _)         = incrementVarError False True
  compile (UnaryOp PreInc (Var var))  =
    [ Load var
    , Const (I 1)
    , VM.Add
    , Store var
    , Load var
    ]
  compile (UnaryOp PreInc _)          = incrementVarError True False
  compile (UnaryOp PostInc (Var var)) =
    [ Load var
    , Load var
    , Const (I 1)
    , VM.Add
    , Store var
    ]
  compile (UnaryOp PostInc _)         = incrementVarError False False
  compile (UnaryOp op expr)           = compile expr ++ compile op
  compile (BinaryOp op e1 e2)         = compile e1 ++ compile e2 ++ compile op

instance Compilable UnaryOp where
  compile AST.Neg     = [VM.Neg]
  compile AST.Not     = [VM.Not]
  compile AST.PreDec  = incrementVarError True True
  compile AST.PostDec = incrementVarError False True
  compile AST.PreInc  = incrementVarError True False
  compile AST.PostInc = incrementVarError False False

instance Compilable BinaryOp where
  compile AST.Add     = [VM.Add]
  compile AST.Sub     = [VM.Sub]
  compile AST.Mul     = [VM.Mul]
  compile AST.Div     = [VM.Div]
  compile AST.Mod     = [VM.Mod]
  compile AST.Exp     = [VM.Exp]
  compile AST.Divides = [VM.Divides]
  compile AST.Eq      = [VM.Eq]
  compile AST.Ineq    = [VM.Ineq]
  compile AST.Lt      = [VM.Lt]
  compile AST.Gt      = [VM.Gt]
  compile AST.LtEq    = [VM.LtEq]
  compile AST.GtEq    = [VM.GtEq]
  compile AST.And     = [VM.And]
  compile AST.Or      = [VM.Or]

compileError :: String -> a
compileError = error . ("Compile Error: "++)

incrementVarError :: Bool -> Bool -> a
incrementVarError pre dec = compileError $
  "Can only " ++ (if pre then "pre" else "post") ++ "-" ++
  (if dec then "dec" else "inc") ++ "rement variables"
