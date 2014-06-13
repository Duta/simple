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
                                        ( errors
                                        $ foldl
                                          (\(TypecheckResults errs m) stmt
                                            -> let (TypecheckResults errs' m') = typecheck m stmt
                                            in TypecheckResults (errs ++ errs') m')
                                          (TypecheckResults [] m)
                                          stmts
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
  typecheck m (Init varType var expr p) = TypecheckResults
                                        ( getErrors
                                        $ expectingType m expr varType
                                        ) (M.insert var varType m)
  typecheck m (Expr expr p)             = typecheck m expr



instance Typecheckable Expr where
  typecheck m (Set var expr p)       = TypecheckResults
                                     ( maybe [] (getErrors . expectingType m expr)
                                     $ M.lookup var m
                                     ) m
  typecheck m (FuncCall func args p) = TypecheckResults [] m -- TODO
  typecheck m expr@UnaryOp{}         = TypecheckResults (getResolutionErrors m expr) m
  typecheck m expr@BinaryOp{}        = TypecheckResults (getResolutionErrors m expr) m
  typecheck m _                      = TypecheckResults [] m

type ResolvedType = Either [TypeError] Type

type TypeMap = M.Map Identifier Type

getErrors :: ResolvedType -> [TypeError]
getErrors = either id $ const []

getResolutionErrors :: TypeMap -> Expr -> [TypeError]
getResolutionErrors m = getErrors . resolveType m

ensureType :: Expr -> Type -> Type -> ResolvedType
ensureType expr expected actual = if expected == actual
  then Right expected
  else Left [TypeError expr expected actual]

resolveType :: TypeMap -> Expr -> ResolvedType
resolveType m (Set var expr p)       = maybe (Left []) Right
                                     $ M.lookup var m
resolveType m (FuncCall func args p) = Left [] -- TODO
resolveType m (Var var p)            = maybe (Left []) Right
                                     $ M.lookup var m
resolveType m (IntLit int p)         = Right Int
resolveType m (BoolLit bool p)       = Right Bool
resolveType m (UnaryOp op p expr)    = resolveUnaryOpType m op expr
resolveType m (BinaryOp op p e1 e2)  = resolveBinaryOpType m op e1 e2

expectingType :: TypeMap -> Expr -> Type -> ResolvedType
expectingType m expr expected = resolveType m expr >>= ensureType expr expected

resolveUnaryOpType :: TypeMap -> UnaryOp -> Expr -> ResolvedType
resolveUnaryOpType m Neg     expr = expectingType m expr Int
resolveUnaryOpType m Not     expr = expectingType m expr Bool
resolveUnaryOpType m PreDec  expr = expectingType m expr Int
resolveUnaryOpType m PostDec expr = expectingType m expr Int
resolveUnaryOpType m PreInc  expr = expectingType m expr Int
resolveUnaryOpType m PostInc expr = expectingType m expr Int

-- TODO: If both expressions have type errors, concat the two lists
resolveBinaryOpType :: TypeMap -> BinaryOp -> Expr -> Expr -> ResolvedType
resolveBinaryOpType m Add     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Sub     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Mul     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Div     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Mod     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Exp     e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Divides e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int
resolveBinaryOpType m Eq      e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m Ineq    e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m Lt      e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m Gt      e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m LtEq    e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m GtEq    e1 e2 = expectingType m e1 Int  >> expectingType m e2 Int  >> return Bool
resolveBinaryOpType m And     e1 e2 = expectingType m e1 Bool >> expectingType m e2 Bool
resolveBinaryOpType m Or      e1 e2 = expectingType m e1 Bool >> expectingType m e2 Bool
