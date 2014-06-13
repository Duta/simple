module Simple.Typechecker where

import           Data.List (intercalate)
import qualified Data.Map                          as M
import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST

data TypeError = TypeError
                 { expr     :: Expr
                 , expected :: Type
                 , actual   :: Type
                 } deriving (Show, Eq)

check :: Stmt -> Stmt
check stmt
  | null typeErrors = stmt
  | otherwise       = error
                    . intercalate "\n"
                    . (["Type errors:"]++)
                    . map repr
                    $ typeErrors
    where
      typeErrors = errors $ typecheck M.empty stmt
      repr typeError
        = showString " - Expected: "
        . shows (expected typeError)
        . showString "\n   Actual:   "
        . shows (actual typeError)
        . showString "\n   @ "
        . showString name
        . showString " "
        . shows l1
        . showString ":"
        . shows c1
        . showString "-"
        . shows l2
        . showString ":"
        . shows c2
        $ ""
          where
            p = source $ expr typeError
            name = sourceName   $ start p
            l1   = sourceLine   $ start p
            c1   = sourceColumn $ start p
            l2   = sourceLine   $ end   p
            c2   = sourceColumn $ end   p

data TypecheckResults = TypecheckResults
                      { errors  :: [TypeError]
                      , typeMap :: TypeMap
                      } deriving (Show, Eq)

class Typecheckable a where
  typecheck :: TypeMap -> a -> TypecheckResults

instance Typecheckable Stmt where
  typecheck m (Seq stmts p)             = TypecheckResults
                                        ( concatMap (errors . typecheck m) stmts
                                        ) m
  typecheck m (While cond stmts p)      = TypecheckResults
                                        ( errors (typecheck m cond)
                                       ++ errors (typecheck m stmts)
                                        ) m
  typecheck m (IfElse cond s1 s2 p)     = TypecheckResults
                                        ( errors (typecheck m cond)
                                       ++ errors (typecheck m s1)
                                       ++ errors (typecheck m s2)
                                        ) m
  typecheck m (If cond stmts p)         = TypecheckResults
                                        ( errors (typecheck m cond)
                                       ++ errors (typecheck m stmts)
                                        ) m
  typecheck m (Init varType var expr p) = TypecheckResults [] m -- TODO
  typecheck m (Expr expr p)             = typecheck m expr

instance Typecheckable Expr where
  typecheck m (Set var expr p)       = TypecheckResults [] m -- TODO
  typecheck m (FuncCall func args p) = TypecheckResults [] m -- TODO
  typecheck m expr@UnaryOp{}         = TypecheckResults
                                     ( either id (const []) $ resolveType expr
                                     ) m
  typecheck m expr@BinaryOp{}        = TypecheckResults
                                     ( either id (const []) $ resolveType expr
                                     ) m
  typecheck m _                      = TypecheckResults [] m

type ResolvedType = Either [TypeError] Type

type TypeMap = M.Map Identifier Type

ensureType :: Expr -> Type -> Type -> ResolvedType
ensureType expr expected actual = if expected == actual
  then Right expected
  else Left [TypeError expr expected actual]

resolveType :: Expr -> ResolvedType
resolveType (Set var expr p)       = Left [] -- TODO
resolveType (FuncCall func args p) = Left [] -- TODO
resolveType (Var var p)            = Left [] -- TODO
resolveType (IntLit int p)         = Right Int
resolveType (BoolLit bool p)       = Right Bool
resolveType (UnaryOp op p expr)    = resolveUnaryOpType op expr
resolveType (BinaryOp op p e1 e2)  = resolveBinaryOpType op e1 e2

expectingType :: Expr -> Type -> ResolvedType
expectingType expr expected = resolveType expr >>= ensureType expr expected

resolveUnaryOpType :: UnaryOp -> Expr -> ResolvedType
resolveUnaryOpType Neg     expr = expectingType expr Int
resolveUnaryOpType Not     expr = expectingType expr Bool
resolveUnaryOpType PreDec  expr = expectingType expr Int
resolveUnaryOpType PostDec expr = expectingType expr Int
resolveUnaryOpType PreInc  expr = expectingType expr Int
resolveUnaryOpType PostInc expr = expectingType expr Int

-- TODO: If both expressions have type errors, concat the two lists
resolveBinaryOpType :: BinaryOp -> Expr -> Expr -> ResolvedType
resolveBinaryOpType Add     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Sub     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Mul     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Div     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Mod     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Exp     e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Divides e1 e2 = expectingType e1 Int  >> expectingType e2 Int
resolveBinaryOpType Eq      e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType Ineq    e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType Lt      e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType Gt      e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType LtEq    e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType GtEq    e1 e2 = expectingType e1 Int  >> expectingType e2 Int  >> return Bool
resolveBinaryOpType And     e1 e2 = expectingType e1 Bool >> expectingType e2 Bool
resolveBinaryOpType Or      e1 e2 = expectingType e1 Bool >> expectingType e2 Bool
