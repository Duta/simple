module Simple.VM (Instruction(..), Value(..), Bytecode, execute) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (foldM, void)
import qualified Data.Map as M

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
  | Divides
  | Eq
  | Ineq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | And
  | Or
  | Flip
  | Print
  | If
  | While
  | Store String
  | Load String
    deriving (Show, Eq, Read)

type Bytecode = [Instruction]
type Stack = [Value]
type Vars = M.Map String Value
type Memory = (Stack, Vars)

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
repr (Code _)  = typeError

vmError :: String -> a
vmError = error . ("Runtime Error: "++)

typeError :: a
typeError = vmError "Type error"

stackUnderflowError :: a
stackUnderflowError = vmError "Stack underflow"

undefVarError :: String -> a
undefVarError = vmError . ("Attempted to access undefined variable " ++)

unOpInt :: Memory -> (Int -> Value) -> IO Memory
unOpInt (I a:s, v) op = return (op a:s, v)
unOpInt (_:_, _)   op = typeError
unOpInt _          op = stackUnderflowError

unOpBool :: Memory -> (Bool -> Value) -> IO Memory
unOpBool (B a:s, v) op = return (op a:s, v)
unOpBool (_:_, _)   op = typeError
unOpBool _          op = stackUnderflowError

binOpInt :: Memory -> (Int -> Int -> Value) -> IO Memory
binOpInt (I b:I a:s, v) op = return (a `op` b:s, v)
binOpInt (_:_:_, _)     op = typeError
binOpInt _              op = stackUnderflowError

binOpBool :: Memory -> (Bool -> Bool -> Value) -> IO Memory
binOpBool (B b:B a:s, v) op = return (a `op` b:s, v)
binOpBool (_:_:_, _)     op = typeError
binOpBool _              op = stackUnderflowError

exeIf :: Memory -> IO Memory
exeIf (B b:Code c2:Code c1:s, v) = exeCode (s, v) (if b then c1 else c2)
exeIf (_:_:_, _)                 = typeError
exeIf _                          = stackUnderflowError

exeWhile :: Memory -> IO Memory
exeWhile (Code e:Code c:s, v) = do
  m' <- exeCode ([], v) e
  case m' of
    ([B b], v') -> if b
      then exeCode (s, v') c >>= (\(s', v'') -> exeWhile (Code e:Code c:s', v''))
      else return (s, v')
    (_:_, _)    -> typeError
    _           -> stackUnderflowError
exeWhile (_:_, _)             = typeError
exeWhile _                    = stackUnderflowError

exeIns :: Memory -> Instruction -> IO Memory
exeIns (s, v)     (Const n) = return (n:s, v)
exeIns m          Neg       = unOpInt   m $ I . negate
exeIns m          Not       = unOpBool  m $ B . not
exeIns m          Add       = binOpInt  m $ \a b -> I (a + b)
exeIns m          Sub       = binOpInt  m $ \a b -> I (a - b)
exeIns m          Mul       = binOpInt  m $ \a b -> I (a * b)
exeIns m          Div       = binOpInt  m $ \a b -> I (a `div` b)
exeIns m          Mod       = binOpInt  m $ \a b -> I (a `mod` b)
exeIns m          Exp       = binOpInt  m $ \a b -> I (a ^ b)
exeIns m          Divides   = binOpInt  m $ \a b -> B (b `mod` a == 0)
exeIns m          Eq        = binOpInt  m $ \a b -> B (a == b)
exeIns m          Ineq      = binOpInt  m $ \a b -> B (a /= b)
exeIns m          Lt        = binOpInt  m $ \a b -> B (a < b)
exeIns m          Gt        = binOpInt  m $ \a b -> B (a > b)
exeIns m          LtEq      = binOpInt  m $ \a b -> B (a <= b)
exeIns m          GtEq      = binOpInt  m $ \a b -> B (a >= b)
exeIns m          And       = binOpBool m $ \a b -> B (a && b)
exeIns m          Or        = binOpBool m $ \a b -> B (a || b)
exeIns (b:a:s, v) Flip      = return (a:b:s, v)
exeIns _          Flip      = stackUnderflowError
exeIns (n:s, v)   Print     = putStrLn (repr n) >> return (s, v)
exeIns _          Print     = stackUnderflowError
exeIns m          If        = exeIf m
exeIns m          While     = exeWhile m
exeIns (n:s, v)   (Store i) = return (s, M.insert i n v)
exeIns _          (Store i) = stackUnderflowError
exeIns (s, v)     (Load i)  = case M.lookup i v of
  (Just n) -> return (n:s, v)
  Nothing -> undefVarError i

exeCode :: Memory -> Bytecode -> IO Memory
exeCode = foldM exeIns

initialMem :: Memory
initialMem = ([], M.empty)

execute :: Bytecode -> IO ()
execute = void . exeCode initialMem

-- Takes two variable names and swaps their values.
swap :: String -> String -> Bytecode
swap a b = [
  Load a,
  Load b,
  Store a,
  Store b]
