module Simple.Typechecker (check) where

import           Data.List (intercalate)
import qualified Data.Map                          as M
import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST

check :: Stmt -> Stmt
check stmt
  | null typeErrors = stmt
  | otherwise       = error
                    . intercalate "\n"
                    . (["Type errors:"]++)
                    . map repr
                    $ typeErrors
    where
      typeErrors = errors $ typecheck M.empty Nothing stmt
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
            p = source typeError
            name = sourceName   $ start p
            l1   = sourceLine   $ start p
            c1   = sourceColumn $ start p
            l2   = sourceLine   $ end   p
            c2   = sourceColumn $ end   p

data TypeError
  = TypeError
  { errSource :: Source
  , expected  :: Type
  , actual    :: Type
  } deriving (Show, Eq)

instance HasSource TypeError where
  source = errSource

data TypecheckResults = TypecheckResults
                      { errors  :: [TypeError]
                      , typeMap :: TypeMap
                      } deriving (Show, Eq)

type ReturnType = Maybe Type

class Typecheckable a where
  typecheck :: TypeMap -> ReturnType -> a -> TypecheckResults

instance Typecheckable Stmt where
  typecheck m r (Seq stmts p)              = foldl
                                             (\(TypecheckResults errs m) stmt
                                               -> let (TypecheckResults errs' m') = typecheck m r stmt
                                               in TypecheckResults (errs ++ errs') m')
                                             (TypecheckResults [] m)
                                             stmts
  typecheck m r (While cond stmts p)       = TypecheckResults
                                           ( getTypecheckErrors m r cond
                                          ++ getExpectedTypeErrors m r cond Bool
                                          ++ getTypecheckErrors m r stmts
                                           ) m
  typecheck m r (For ini cond inc stmts p) = let m' = typeMap $ typecheck m r ini
                                          in TypecheckResults
                                           ( getTypecheckErrors m' r cond
                                          ++ getExpectedTypeErrors m' r cond Bool
                                          ++ getTypecheckErrors m' r inc
                                          ++ getTypecheckErrors m' r stmts
                                           ) m'
  typecheck m r (IfElse cond s1 s2 p)      = TypecheckResults
                                           ( getTypecheckErrors m r cond
                                          ++ getExpectedTypeErrors m r cond Bool
                                          ++ getTypecheckErrors m r s1
                                          ++ getTypecheckErrors m r s2
                                           ) m
  typecheck m r (If cond stmts p)          = TypecheckResults
                                           ( getTypecheckErrors m r cond
                                          ++ getExpectedTypeErrors m r cond Bool
                                          ++ getTypecheckErrors m r stmts
                                           ) m
  typecheck m r (Init varType var expr p)  = TypecheckResults 
                                           ( getExpectedTypeErrors m r expr varType
                                          ++ getTypecheckErrors m r expr
                                           )$M.insert var varType m
  typecheck m r (Decl varType var p)       = TypecheckResults []
                                           $ M.insert var varType m
  typecheck m r (Expr expr p)              = typecheck m r expr
  typecheck m r (Return expr p)            = TypecheckResults
                                           ( maybe [] (\retType -> maybe 
                                               (getErrors $ ensureType p retType Void)
                                               (\e -> getTypecheckErrors m r e ++ getExpectedTypeErrors m r e retType)
                                               expr) r
                                           ) m

--maybe
--  []
--  (\returnType ->
--    maybe 
--      (getErrors $ ensureType retExpr Void returnType)
--      (\e -> getTypecheckErrors m r e ++ getExpectedTypeErrors m r e returnType))
--      expr
--  r

instance Typecheckable Expr where
  typecheck m r (Set var expr p)                = TypecheckResults
                                                ((maybe [] (getExpectedTypeErrors m r expr)
                                                $ M.lookup var m)
                                               ++ getTypecheckErrors m r expr
                                                ) m
  typecheck m r (FuncCall func args p)          = TypecheckResults [] m -- TODO
  typecheck m r (Lambda funcType params body p) = let m' = foldl (\m (n,t) -> M.insert n t m) m (zip params $ paramTypes funcType)
                                               in TypecheckResults
                                                ( getTypecheckErrors m (Just $ retType funcType) body
                                                ) m
  typecheck m r expr@UnaryOp{}                  = TypecheckResults (getResolutionErrors m r expr) m
  typecheck m r expr@BinaryOp{}                 = TypecheckResults (getResolutionErrors m r expr) m
  typecheck m r _                               = TypecheckResults [] m

instance Typecheckable FuncBody where
  typecheck m r (ExprBody expr) = typecheck m r expr
  typecheck m r (StmtBody stmt) = typecheck m r stmt

type ResolvedType = Either [TypeError] Type

type TypeMap = M.Map Identifier Type

getErrors :: ResolvedType -> [TypeError]
getErrors = either id $ const []

getResolutionErrors :: TypeMap -> ReturnType -> Expr -> [TypeError]
getResolutionErrors m r = getErrors . resolveType m r

getTypecheckErrors :: Typecheckable a => TypeMap -> ReturnType -> a -> [TypeError]
getTypecheckErrors m r = errors . typecheck m r

getExpectedTypeErrors :: TypeMap -> ReturnType -> Expr -> Type -> [TypeError]
getExpectedTypeErrors m r expr = getErrors . expectingType m r expr

ensureType :: Source -> Type -> Type -> ResolvedType
ensureType p expected actual = if expected == actual
  then Right expected
  else Left [TypeError p expected actual]

resolveType :: TypeMap -> ReturnType -> Expr -> ResolvedType
resolveType m r (Set var expr p)                = maybe (Left []) Right
                                                $ M.lookup var m
resolveType m r (FuncCall func args p)          = Left -- TODO: Check arg types match param types
                                                $ concatMap (getTypecheckErrors m r) args
resolveType m r (Lambda funcType params body p) = Right Func
resolveType m r (Var var p)                     = maybe (Left []) Right
                                                $ M.lookup var m
resolveType m r (IntLit int p)                  = Right Int
resolveType m r (BoolLit bool p)                = Right Bool
resolveType m r (UnaryOp op p expr)             = resolveUnaryOpType m r op expr
resolveType m r (BinaryOp op p e1 e2)           = resolveBinaryOpType m r op e1 e2

expectingType :: TypeMap -> ReturnType -> Expr -> Type -> ResolvedType
expectingType m r expr expected = resolveType m r expr >>= ensureType (source expr) expected

resolveUnaryOpType :: TypeMap -> ReturnType -> UnaryOp -> Expr -> ResolvedType
resolveUnaryOpType m r Neg     expr = expectingType m r expr Int
resolveUnaryOpType m r Not     expr = expectingType m r expr Bool
resolveUnaryOpType m r PreDec  expr = expectingType m r expr Int
resolveUnaryOpType m r PostDec expr = expectingType m r expr Int
resolveUnaryOpType m r PreInc  expr = expectingType m r expr Int
resolveUnaryOpType m r PostInc expr = expectingType m r expr Int

-- TODO: If both expressions have type errors, concat the two lists
resolveBinaryOpType :: TypeMap -> ReturnType -> BinaryOp -> Expr -> Expr -> ResolvedType
resolveBinaryOpType m r Add     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Sub     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Mul     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Div     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Mod     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Exp     e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int
resolveBinaryOpType m r Divides e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r Eq      e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r Ineq    e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r Lt      e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r Gt      e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r LtEq    e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r GtEq    e1 e2 = expectingType m r e1 Int  >> expectingType m r e2 Int  >> return Bool
resolveBinaryOpType m r And     e1 e2 = expectingType m r e1 Bool >> expectingType m r e2 Bool
resolveBinaryOpType m r Or      e1 e2 = expectingType m r e1 Bool >> expectingType m r e2 Bool
