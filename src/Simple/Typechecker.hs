module Simple.Typechecker where

import Simple.AST

type TypeError = (Identifier, Source)

check :: Stmt -> Stmt
check stmt
  | null errors = stmt
  | otherwise   = undefined
    where errors = typecheck stmt

class Typecheckable a where
  typecheck :: a -> [TypeError]

instance Typecheckable Stmt where
  typecheck (Seq stmts p)                = concatMap typecheck stmts
  typecheck (While cond stmts p)         = [] -- TODO
  typecheck (IfElse cond stmts1 stmt2 p) = [] -- TODO
  typecheck (If cond stmt p)             = [] -- TODO
  typecheck (Init varType var expr p)    = [] -- TODO
  typecheck (Expr expr p)                = [] -- TODO

instance Typecheckable Expr where
  typecheck (Set var expr p)       = [] -- TODO
  typecheck (FuncCall func args p) = [] -- TODO
  typecheck (Var var p)            = [] -- TODO
  typecheck (IntLit int p)         = [] -- TODO
  typecheck (BoolLit bool p)       = [] -- TODO
  typecheck (UnaryOp op p expr)    = [] -- TODO
  typecheck (BinaryOp op p e1 e2)  = [] -- TODO
