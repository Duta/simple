module Simple.Primitives where

import           Simple.AST

defaultExpr :: Type -> Expr
defaultExpr Int  = IntLit 0 undefined
defaultExpr Bool = BoolLit False undefined
