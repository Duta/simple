module Simple.Typechecker where

import           Data.List (intercalate)
import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST

data TypeError = TypeError
                 { expr     :: Expr
                 , expected :: Type
                 , actual   :: Type
                 } deriving (Show, Eq)

check :: Stmt -> Stmt
check stmt
  | null errors = stmt
  | otherwise   = error
                . intercalate "\n"
                . (["Type errors:"]++)
                . map repr
                $ errors
    where
      errors = typecheck stmt
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

class Typecheckable a where
  typecheck :: a -> [TypeError]

typecheckAll :: Typecheckable a => [a] -> [TypeError]
typecheckAll = concatMap typecheck

instance Typecheckable Stmt where
  typecheck (Seq stmts p)             = typecheckAll stmts
  typecheck (While cond stmts p)      = typecheck cond
                                     ++ typecheck stmts
  typecheck (IfElse cond s1 s2 p)     = typecheck cond
                                     ++ typecheck s1
                                     ++ typecheck s2
  typecheck (If cond stmt p)          = typecheck cond
                                     ++ typecheck stmt
  typecheck (Init varType var expr p) = [] -- TODO
  typecheck (Expr expr p)             = typecheck expr

instance Typecheckable Expr where
  typecheck (Set var expr p)       = [] -- TODO
  typecheck (FuncCall func args p) = [] -- TODO
  typecheck expr@UnaryOp{}         = either id (const []) $ resolveType expr
  typecheck expr@BinaryOp{}        = either id (const []) $ resolveType expr
  typecheck _                      = []

type ResolvedType = Either [TypeError] Type

ensureType :: Expr -> Type -> Type -> ResolvedType
ensureType expr expected actual = if expected == actual
  then Right expected
  else Left [TypeError expr expected actual]

resolveType :: Expr -> ResolvedType
resolveType (Set var expr p)       = Left []
resolveType (FuncCall func args p) = Left []
resolveType (Var var p)            = Left []
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
