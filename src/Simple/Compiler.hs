module Simple.Compiler where

import           Simple.AST
import           Simple.VM

class Compilable a where
  compile :: a -> Bytecode

instance Compilable Stmt where
  compile (Seq stmts)                 = concatMap compile stmts
  compile (While cond stmts)          = undefined
  compile (IfElse cond stmts1 stmts2) = undefined
  compile (If cond stmts)             = undefined
  compile (Init varType var expr)     = compile $ Set var expr
  compile (Expr expr)                 = undefined

instance Compilable Expr where
  compile (Set var expr)       = compile expr ++ [Store var]
  compile (FuncCall func args) = undefined
  compile (Var var)            = undefined
  compile (IntLit int)         = undefined
  compile (BoolLit bool)       = undefined
  compile (UnaryOp op expr)    = undefined
  compile (BinaryOp op e1 e2)  = undefined
