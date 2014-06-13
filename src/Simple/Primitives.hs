module Simple.Primitives (defaultExpr) where

import           Simple.AST

defaultExpr :: Type -> Expr
defaultExpr Int  = IntLit  0     errorMsg
defaultExpr Bool = BoolLit False errorMsg

errorMsg = error $ "Attempted to access source "
                ++ "position of default expression"
