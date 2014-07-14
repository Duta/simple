module Simple.Typechecker (check) where

import           Data.List (intercalate)
import qualified Data.Map                          as M
import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST
import           Simple.ReducedAST

check :: Stmt -> Stmt
check stmt
  | null typeErrors = stmt
  | otherwise       = error
                    . intercalate "\n"
                    . (["Type errors:"]++)
                    . map repr
                    $ typeErrors
    where
      noInfo = TypeInfo M.empty M.empty Nothing
      typeErrors = errors $ typecheck noInfo stmt
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

type TypeMap    = M.Map Identifier Type
type ReturnType = Maybe Type
type FuncMap    = M.Map Identifier FuncType

data TypeInfo
  = TypeInfo
  { varMap  :: TypeMap
  , funcMap :: FuncMap
  , retType :: ReturnType
  } deriving (Show, Eq)

data TypecheckResults = TypecheckResults
                      { errors   :: [TypeError]
                      , typeInfo :: TypeInfo
                      } deriving (Show, Eq)

class Typecheckable a where
  typecheck :: TypeInfo -> a -> TypecheckResults

instance Typecheckable Stmt where
  typecheck i (Seq stmts p)              = foldl
                                           (\(TypecheckResults errs i) stmt
                                             -> let (TypecheckResults errs' i') = typecheck i stmt
                                             in TypecheckResults (errs ++ errs') i')
                                           (TypecheckResults [] i)
                                           stmts
  typecheck i (While cond stmts p)       = TypecheckResults
                                         ( getTypecheckErrors i cond
                                        ++ getExpectedTypeErrors i cond Bool
                                        ++ getTypecheckErrors i stmts
                                         ) i
  typecheck i (For ini cond inc stmts p) = let m' = varMap . typeInfo $ typecheck i ini
                                               i' = i {varMap = m'}
                                        in TypecheckResults
                                         ( getTypecheckErrors i' cond
                                        ++ getExpectedTypeErrors i' cond Bool
                                        ++ getTypecheckErrors i' inc
                                        ++ getTypecheckErrors i' stmts
                                         ) i'
  typecheck i (IfElse cond s1 s2 p)      = TypecheckResults
                                         ( getTypecheckErrors i cond
                                        ++ getExpectedTypeErrors i cond Bool
                                        ++ getTypecheckErrors i s1
                                        ++ getTypecheckErrors i s2
                                         ) i
  typecheck i (Init varType var expr p)  = let funcType = getFuncType expr
                                               noLambdaErrors = case funcType of Right _ -> True; _ -> False
                                        in TypecheckResults 
                                         ( getExpectedTypeErrors i expr varType
                                        ++ getTypecheckErrors i expr
                                        ++ case funcType of Right _ -> []; Left e -> e
                                         )$i {varMap  = M.insert var varType $ varMap i
                                             ,funcMap = if varType == Func && noLambdaErrors
                                                        then let Right ft = funcType
                                                          in (M.insert var ft $ funcMap i)
                                                        else funcMap i}
  typecheck i (Expr expr p)              = typecheck i expr
  typecheck i (Return expr p)            = let r = retType i
                                        in TypecheckResults
                                         ( maybe [] (\retType -> maybe 
                                             (getErrors $ ensureType p retType Void)
                                             (\e -> getTypecheckErrors i e ++ getExpectedTypeErrors i e retType)
                                             expr) r
                                         ) i

instance Typecheckable Expr where
  typecheck i (Set var expr p)                = TypecheckResults
                                                ((maybe [] (getExpectedTypeErrors i expr)
                                                . M.lookup var $ varMap i)
                                               ++ getTypecheckErrors i expr
                                                ) i
  typecheck i (FuncCall func args p)          = TypecheckResults
                                                ( concatMap (getTypecheckErrors i) args
                                            -- ++ -- TODO: Check arg types match
                                                ) i
  typecheck i (Lambda funcType params body p) = let m' = foldl (\m (n,t) -> M.insert n t m) (varMap i) . zip params $ paramTypes funcType
                                               in TypecheckResults
                                                ( getTypecheckErrors (i {retType = Just $ funcRetType funcType}) body
                                                ) $ i {varMap = m'}
  typecheck i expr@UnaryOp{}                  = TypecheckResults (getResolutionErrors i expr) i
  typecheck i expr@BinaryOp{}                 = TypecheckResults (getResolutionErrors i expr) i
  typecheck i _                               = TypecheckResults [] i

instance Typecheckable FuncBody where
  typecheck i (ExprBody expr) = typecheck i expr
  typecheck i (StmtBody stmt) = typecheck i stmt

type Resolved a = Either [TypeError] a

getFuncType :: Expr -> Resolved FuncType
getFuncType (Lambda t _ _ _) = return t
getFuncType _                = Left []

getErrors :: Resolved Type -> [TypeError]
getErrors = either id $ const []

getResolutionErrors :: TypeInfo -> Expr -> [TypeError]
getResolutionErrors i = getErrors . resolveType i

getTypecheckErrors :: Typecheckable a => TypeInfo -> a -> [TypeError]
getTypecheckErrors i = errors . typecheck i

getExpectedTypeErrors :: TypeInfo -> Expr -> Type -> [TypeError]
getExpectedTypeErrors i expr = getErrors . expectingType i expr

ensureType :: Source -> Type -> Type -> Resolved Type
ensureType p expected actual = if expected == actual
  then return expected
  else Left [TypeError p expected actual]

resolveType :: TypeInfo -> Expr -> Resolved Type
resolveType i (Set var expr p)                = maybe (Left []) return . M.lookup var $ varMap i
resolveType i (FuncCall func args p)          = maybe (Left []) (return . funcRetType) . M.lookup func $ funcMap i
resolveType i (Lambda funcType params body p) = return Func
resolveType i (Var var p)                     = maybe (Left []) return . M.lookup var $ varMap i
resolveType i (IntLit int p)                  = return Int
resolveType i (BoolLit bool p)                = return Bool
resolveType i (UnaryOp op p expr)             = resolveUnaryOpType i op expr
resolveType i (BinaryOp op p e1 e2)           = resolveBinaryOpType i op e1 e2

expectingType :: TypeInfo -> Expr -> Type -> Resolved Type
expectingType i expr expected = resolveType i expr >>= ensureType (source expr) expected

resolveUnaryOpType :: TypeInfo -> UnaryOp -> Expr -> Resolved Type
resolveUnaryOpType i Neg     expr = expectingType i expr Int
resolveUnaryOpType i Not     expr = expectingType i expr Bool
resolveUnaryOpType i PreDec  expr = expectingType i expr Int
resolveUnaryOpType i PostDec expr = expectingType i expr Int
resolveUnaryOpType i PreInc  expr = expectingType i expr Int
resolveUnaryOpType i PostInc expr = expectingType i expr Int

-- TODO: If both expressions have type errors, concat the two lists
resolveBinaryOpType :: TypeInfo -> BinaryOp -> Expr -> Expr -> Resolved Type
resolveBinaryOpType i Add     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Sub     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Mul     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Div     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Mod     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Exp     e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int
resolveBinaryOpType i Eq      e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i Ineq    e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i Lt      e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i Gt      e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i LtEq    e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i GtEq    e1 e2 = expectingType i e1 Int  >> expectingType i e2 Int  >> return Bool
resolveBinaryOpType i And     e1 e2 = expectingType i e1 Bool >> expectingType i e2 Bool
resolveBinaryOpType i Or      e1 e2 = expectingType i e1 Bool >> expectingType i e2 Bool
