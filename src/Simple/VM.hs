module Simple.VM (Instruction(..), Value(..), Bytecode, execute) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad   (foldM, void)
import qualified Data.Map        as M
import           Data.List       (intercalate)

data Value
  = B Bool
  | I Int
  | Code Bytecode
    deriving (Show, Eq, Read)

data Instruction
  = Const Value
  | Neg
  | Not
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Exp
  | Eq
  | Ineq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | And
  | Or
  | Print
  | If
  | While
  | Exec
  | Return
  | RMStack
  | Store String
  | Load String
    deriving (Show, Eq, Read)

type Bytecode = [Instruction]
type Stack    = [Value]
type Vars     = M.Map String Value
type Memory   = (Stack, Vars)

instance NFData Instruction where
  rnf (Const v) = rnf v
  rnf a         = a `seq` ()

instance NFData Value where
  rnf (Code c) = rnf c
  rnf a        = a `seq` ()

repr :: Value -> String
repr (B True)  = "true"
repr (B False) = "false"
repr (I n)     = show n
repr (Code _)  = typeError "Repr" initialMem

vmError :: String -> a
vmError = error . ("Runtime Error: "++)

typeError :: String -> Memory -> a
typeError loc mem = vmError $ intercalate "\n"
  [ "Type error"
  , "  Loc: " ++ loc
  , "  Mem: " ++ show mem
  ]

stackUnderflowError :: String -> Memory -> a
stackUnderflowError loc mem = vmError $ intercalate "\n"
  [ "Stack underflow"
  , "  Loc: " ++ loc
  , "  Mem: " ++ show mem
  ]

undefVarError :: String -> a
undefVarError = vmError . ("Attempted to access undefined variable " ++)

unOpInt :: Memory -> (Int -> Value) -> IO Memory
unOpInt (I a:s, v) op = return (op a:s, v)
unOpInt m@(_:_, _) op = typeError           "UnOpInt" m
unOpInt m          op = stackUnderflowError "UnOpInt" m

unOpBool :: Memory -> (Bool -> Value) -> IO Memory
unOpBool (B a:s, v) op = return (op a:s, v)
unOpBool m@(_:_, _) op = typeError           "UnOpBool" m
unOpBool m          op = stackUnderflowError "UnOpBool" m

binOpInt :: Memory -> (Int -> Int -> Value) -> IO Memory
binOpInt (I b:I a:s, v) op = return (a `op` b:s, v)
binOpInt m@(_:_:_, _)   op = typeError           "BinOpInt" m
binOpInt m              op = stackUnderflowError "BinOpInt" m

binOpBool :: Memory -> (Bool -> Bool -> Value) -> IO Memory
binOpBool (B b:B a:s, v) op = return (a `op` b:s, v)
binOpBool m@(_:_:_, _)   op = typeError           "BinOpBool" m
binOpBool m              op = stackUnderflowError "BinOpBool" m

exeIf :: (Memory -> Bytecode -> IO Memory) -> Memory -> IO Memory
exeIf exe (B b:Code c2:Code c1:s, v) = exe (s, v) $ if b then c1 else c2
exeIf _   m@(_:_:_, _)               = typeError           "If" m
exeIf _   m                          = stackUnderflowError "If" m

exeWhile :: (Memory -> Bytecode -> IO Memory) -> Memory -> IO Memory
exeWhile exe (Code e:Code c:s, v) = do
  m' <- exe ([], v) e
  case m' of
    ([B b], v') -> if b
      then do
        (s',v'') <- exe (s, v') c
        exeWhile exe (Code e:Code c:s', v'')
      else return (s, v')
    m@(_:_, _)  -> typeError           "While" m
    m           -> stackUnderflowError "While" m
exeWhile _  m@(_:_, _)           = typeError           "While" m
exeWhile _  m                    = stackUnderflowError "While" m

exeExec :: Memory -> IO Memory
exeExec (Code (Return:_):s, v) = return (s, v)
exeExec (Code (i:c):s, v)      = exeIns (\(s, v) c -> exeExec (Code c:s, v)) (s, v) i >>= (\(s', v') -> exeExec (Code c:s', v'))
exeExec (Code []:s, v)         = return (s, v)
exeExec m@(_:_, _)             = typeError           "Exec" m
exeExec m                      = stackUnderflowError "Exec" m

exeIns :: (Memory -> Bytecode -> IO Memory) -> Memory -> Instruction -> IO Memory
exeIns _   (s, v)     (Const n) = return (n:s, v)
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
exeIns _   (n:s, v)   Print     = putStrLn (repr n) >> return (s, v)
exeIns _   m          Print     = stackUnderflowError "Print" m
exeIns exe m          If        = exeIf    exe m
exeIns exe m          While     = exeWhile exe m
exeIns _   m          Exec      = exeExec m
exeIns _   m          Return    = error "Return outside function call"
exeIns _   (_, v)     RMStack   = return ([], v)
exeIns _   (n:s, v)   (Store i) = return (s, M.insert i n v)
exeIns _   m          (Store i) = stackUnderflowError "Store" m
exeIns _   (s, v)     (Load i)  = case M.lookup i v of
  (Just n) -> return (n:s, v)
  Nothing -> undefVarError i

exeCode :: Memory -> Bytecode -> IO Memory
exeCode = foldM $ exeIns exeCode

initialMem :: Memory
initialMem = ([], M.empty)

execute :: Bytecode -> IO ()
execute = void . exeCode initialMem
