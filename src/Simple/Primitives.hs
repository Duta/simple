module Simple.Primitives (defaultExpr, primitiveFunctions) where

import qualified Data.Map   as M
import           Simple.AST
import           Simple.VM

defaultExpr :: Type -> Expr
defaultExpr Int  = IntLit  0     errorMsg
defaultExpr Bool = BoolLit False errorMsg

errorMsg = error $ "Attempted to access source "
                ++ "position of default expression"

type PrimitiveFunction = (FuncType, Bytecode)

primitiveFunctions :: M.Map String PrimitiveFunction
primitiveFunctions = M.fromList
  [ ( "printInt"
    , (FuncType [Int] Void
    , [Print]))
  , ( "printBool"
    , (FuncType [Bool] Void
    , [Print]))
  ]
-- TODO: Make print polymorphic
