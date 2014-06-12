module Simple.Compiler where

import           Simple.AST as AST
import           Simple.VM  as VM

class Compilable a where
  compile :: a -> Bytecode

instance Compilable Stmt where
  compile (Seq stmts)                 = concatMap compile stmts
  compile (While cond stmts)          = undefined
  compile (IfElse cond stmts1 stmts2) = undefined
  compile (If cond stmts)             = undefined
  compile (Init varType var expr)     = compile $ Set var expr
  compile (Expr expr)                 = compile expr ++ [ClearStack]

instance Compilable Expr where
  compile (Set var expr)       = compile expr ++ [Store var]
  compile (FuncCall func args) = undefined
  compile (Var var)            = [Load var]
  compile (IntLit int)         = [Const $ I int]
  compile (BoolLit bool)       = [Const $ B bool]
  compile (UnaryOp op expr)    = compile expr ++ compile op
  compile (BinaryOp op e1 e2)  = compile e1 ++ compile e2 ++ compile op

instance Compilable UnaryOp where
  compile AST.Neg = [VM.Neg]
  compile AST.Not = [VM.Not]

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
