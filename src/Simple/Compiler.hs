module Simple.Compiler where

import           Simple.AST
import           Simple.VM

compile :: Stmt -> Bytecode
compile (Seq stmts)                 = concatMap compile stmts
compile (While cond stmts)          = undefined
compile (IfElse cond stmts1 stmts2) = undefined
compile (If cond stmts)             = undefined
compile (Init varType var expr)     = undefined
compile (Expr expr)                 = undefined
