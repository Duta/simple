module Simple.Primitives (defaultExpr, primitiveFunctions) where

import qualified Data.Map   as M
import           Simple.AST
import           Simple.ReducedAST
import           Simple.VM

defaultExpr :: Type -> Expr
defaultExpr Int  = IntLit  0     errorMsg
defaultExpr Bool = BoolLit False errorMsg
defaultExpr Void = hasNoDefaultValue "void"
defaultExpr Func = hasNoDefaultValue "func"

errorMsg = error $ "Attempted to access source "
                ++ "position of default expression"

hasNoDefaultValue = error . (++" type has no default value")

type PrimitiveFunction = (FuncType, [Identifier], Bytecode)

primitiveFunctions :: M.Map String PrimitiveFunction
primitiveFunctions = M.fromList
  [ ( "printInt"
    , (FuncType [Int] Void
    , ["__n"]
    , [Load "__n", Print]))
  , ( "printBool"
    , (FuncType [Bool] Void
    , ["__b"]
    , [Load "__b", Print]))
  ]
-- TODO: Make print polymorphic
