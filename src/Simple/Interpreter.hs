module Simple.Interpreter (run) where

import           Control.Monad.State
import           Data.List           (intercalate)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST
import           Simple.ReducedAST

data Val
  = B Bool
  | I Int
  | L [Identifier] FuncBody
  | Null
    deriving (Show, Eq)

type Stack a            = [a]
type Mem                = Stack (M.Map Identifier Val)
type Message            = String
type FuncCall           = ([Identifier], [Expr], FuncBody)
type InterpreterState a = StateT Mem IO a

run :: Stmt -> IO ()
run stmt = void $ runStateT (evalStmt stmt) []

evalStmt :: Stmt -> InterpreterState Val
evalStmt (Seq stmts p) = do
  pushScope
  v <- runStatements stmts
  popScope
  return v
  where
    runStatements [] = return Null
    runStatements (h@(Return{}):t) = evalStmt h
    runStatements (h:t) = evalStmt h >> runStatements t
evalStmt (While cond stmts p) = do
  v <- evalExpr cond
  case v of
    B True -> evalStmt stmts >> evalStmt (While cond stmts p)
    B False -> return Null
    _ -> err p "Non-boolean value in while loop"
evalStmt (IfElse cond s1 s2 p) = do
  v <- evalExpr cond
  case v of
    B True -> evalStmt s1
    B False -> evalStmt s2
    _ -> err p "Non-boolean value in if statement"
evalStmt (Init varType var expr p) = do
  v <- evalExpr expr
  get >>= put . initt p var v
  return Null
evalStmt (Expr expr p) = evalExpr expr >> return Null
evalStmt (Return expr p) = maybe (return Null) evalExpr expr

evalExpr :: Expr -> InterpreterState Val
evalExpr (Set var expr p) = do
  v <- evalExpr expr
  get >>= put . set p var v
  return v
evalExpr (FuncCall func args p) = do
  if func == "printInt"
  then do
    val <- evalExpr $ head args
    case val of
      I n -> lift (print n) >> return Null
      _ -> err p "Argument is not an int"
  else if func == "printBool"
  then do
    val <- evalExpr $ head args
    case val of
      B b -> lift (print b) >> return Null
      _ -> err p "Argument is not a bool"
  else get >>= \m -> case resolve m func p of
    L params body -> runFunction (params, args, body)
    _ -> err p $ "'" ++ func ++ "' is not a function"
evalExpr (Lambda funcType params body p) = return $ L params body
evalExpr (Var var p) = do
  m <- get
  return $ resolve m var p
evalExpr (IntLit int p) = return $ I int
evalExpr (BoolLit bool p) = return $ B bool
evalExpr (UnaryOp op p expr) = unOpFunc op p expr
evalExpr (BinaryOp op p e1 e2) = binOpFunc op p e1 e2

resolve :: Mem -> Identifier -> Source -> Val
resolve []    var p = err p $ "Undefined variable '" ++ var ++ "'"
resolve (m:s) var p = fromMaybe (resolve s var p) $ M.lookup var m

set :: Source -> Identifier -> Val -> Mem -> Mem
set p k v []    = err p $ "Undeclared variable '" ++ k ++ "'"
set p k v (m:s) =
  if k `M.member` m
  then M.insert k v m:s
  else m:set p k v s

initt :: Source -> Identifier -> Val -> Mem -> Mem
initt p k v []    = err p $ "No scope in which to declare '" ++ k ++ "'"
initt p k v (m:s) =
  if k `M.member` m
  then err p $ "Previous declaration of '" ++ k ++ "'"
  else M.insert k v m:s

opVar :: (Identifier -> InterpreterState Val) -> Source -> Expr -> InterpreterState Val
opVar op _ (Var v _) = op v
opVar _  p _         = err p "Not a variable"

opInt :: (Int -> Val) -> Source -> Expr -> InterpreterState Val
opInt op p expr = do
  val <- evalExpr expr
  case val of
    I n -> return $ op n
    _ -> err p "Operand isn't an int"

opBool :: (Bool -> Val) -> Source -> Expr -> InterpreterState Val
opBool op p expr = do
  val <- evalExpr expr
  case val of
    B b -> return $ op b
    _ -> err p "Operand isn't a bool"

opIntInt :: (Int -> Int -> Val) -> Source -> Expr -> Expr -> InterpreterState Val
opIntInt op p e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (I m, I n) -> return $ m `op` n
    _ -> err p "Operands aren't both ints"

opBoolBool :: (Bool -> Bool -> Val) -> Source -> Expr -> Expr -> InterpreterState Val
opBoolBool op p e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (B a, B b) -> return $ a `op` b
    _ -> err p "Operands aren't both bools"

modIntVarOp :: (Int -> Int) -> (Int -> Int) -> Source -> Expr -> InterpreterState Val
modIntVarOp modOp retOp p = opVar (\v -> do
  m <- get
  let val = resolve m v p
  case val of
    I n -> do
      let m' = set p v (I $ modOp n) m
      put m'
      return $ retOp n
  m' <- get
  return $ resolve m' v p) p

unOpFunc :: UnaryOp -> Source -> Expr -> InterpreterState Val
unOpFunc Neg     = opInt $ I . negate
unOpFunc Not     = opBool $ B . not
unOpFunc PreDec  = modIntVarOp (subtract 1) (subtract 1)
unOpFunc PostDec = modIntVarOp (subtract 1) id
unOpFunc PreInc  = modIntVarOp (+1) (+1)
unOpFunc PostInc = modIntVarOp (+1) id

binOpFunc :: BinaryOp -> Source -> Expr -> Expr -> InterpreterState Val
binOpFunc Add  = opIntInt $ \a b -> I (a + b)
binOpFunc Sub  = opIntInt $ \a b -> I (a - b)
binOpFunc Mul  = opIntInt $ \a b -> I (a * b)
binOpFunc Div  = opIntInt $ \a b -> I (a `div` b)
binOpFunc Mod  = opIntInt $ \a b -> I (a `mod` b)
binOpFunc Exp  = opIntInt $ \a b -> I (a ^ b)
binOpFunc Eq   = opIntInt $ \a b -> B (a == b)
binOpFunc Ineq = opIntInt $ \a b -> B (a /= b)
binOpFunc Lt   = opIntInt $ \a b -> B (a < b)
binOpFunc Gt   = opIntInt $ \a b -> B (a > b)
binOpFunc LtEq = opIntInt $ \a b -> B (a <= b)
binOpFunc GtEq = opIntInt $ \a b -> B (a >= b)
binOpFunc And  = opBoolBool $ \a b -> B (a && b)
binOpFunc Or   = opBoolBool $ \a b -> B (a || b)

{-
exeIns _   m          Neg       = unOpInt   m $ I . negate
exeIns _   m          Not       = unOpBool  m $ B . not
exeIns _   m          Add       = binOpInt  m $ \a b -> I (a + b)
exeIns _   m          Sub       = binOpInt  m $ \a b -> I (a - b)
exeIns _   m          Mul       = binOpInt  m $ \a b -> I (a * b)
exeIns _   m          Div       = binOpInt  m $ \a b -> I (a `div` b)
exeIns _   m          Mod       = binOpInt  m $ \a b -> I (a `mod` b)
exeIns _   m          Exp       = binOpInt  m $ \a b -> I (a ^ b)
exeIns _   m          Eq        = binOpInt  m $ \a b -> B (a == b)
exeIns _   m          Ineq      = binOpInt  m $ \a b -> B (a /= b)
exeIns _   m          Lt        = binOpInt  m $ \a b -> B (a < b)
exeIns _   m          Gt        = binOpInt  m $ \a b -> B (a > b)
exeIns _   m          LtEq      = binOpInt  m $ \a b -> B (a <= b)
exeIns _   m          GtEq      = binOpInt  m $ \a b -> B (a >= b)
exeIns _   m          And       = binOpBool m $ \a b -> B (a && b)
exeIns _   m          Or        = binOpBool m $ \a b -> B (a || b)
-}

runFunction :: FuncCall -> InterpreterState Val
runFunction (ps, as, body) = error "Function calls are not implemented"

pushScope :: InterpreterState ()
pushScope = modify (M.empty:)

popScope :: InterpreterState ()
popScope = modify tail -- TODO: Improve error handling for scope underflow

err :: Source -> Message -> a
err p m = error $ intercalate "\n"
  [ "Runtime error:"
  , " - Source position: " ++ pos
  , " - Message: " ++ m
  ] where
    pos = concat [name, " ", show l1, ":", show c1, "-", show l2, ":", show c2]
    name = sourceName   $ start p
    l1   = sourceLine   $ start p
    c1   = sourceColumn $ start p
    l2   = sourceLine   $ end   p
    c2   = sourceColumn $ end   p
