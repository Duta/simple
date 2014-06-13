module Simple.Compiler where

import           Text.ParserCombinators.Parsec.Pos
import           Simple.AST                        as AST
import           Simple.Primitives
import           Simple.VM                         as VM

class Compilable a where
  compile :: a -> Bytecode

instance Compilable Stmt where
  compile (Seq stmts _)              = concatMap compile stmts
  compile (AST.While cond stmts _)   = [Const $ Code (compile stmts)]
                                    ++ [Const $ Code (compile cond)]
                                    ++ [VM.While]
  compile (For ini cond inc stmts p) = compile ini
                                    ++ compile (AST.While cond (Seq [stmts, Expr inc p] p) p)
  compile (IfElse cond s1 s2 _)      = [Const $ Code (compile s1)]
                                    ++ [Const $ Code (compile s2)]
                                    ++ compile cond
                                    ++ [VM.If]
  compile (AST.If cond stmts p)      = compile $ IfElse cond stmts (Seq [] p) p
  compile (Init varType var expr p)  = compile $ Set var expr p
  compile (Decl varType var p)       = compile $ Set var (defaultExpr varType) p
  compile (Expr expr _)              = compile expr ++ [ClearStack]

instance Compilable Expr where
  compile (Set var expr _)                = compile expr ++ [Store var, Load var]
  compile (FuncCall func args _)          = if func == "print"
    then concatMap compile args ++ [Print]
    else error $ "Unknown function " ++ func
  compile (Var var _)                     = [Load var]
  compile (IntLit int _)                  = [Const $ I int]
  compile (BoolLit bool _)                = [Const $ B bool]
  compile (UnaryOp PreDec _ (Var var _))  =
    [ Load var
    , Const (I 1)
    , VM.Sub
    , Store var
    , Load var
    ]
  compile (UnaryOp PreDec p _)            = incrementVarError True True $ Just p
  compile (UnaryOp PostDec _ (Var var _)) =
    [ Load var
    , Load var
    , Const (I 1)
    , VM.Sub
    , Store var
    ]
  compile (UnaryOp PostDec p _)           = incrementVarError False True $ Just p
  compile (UnaryOp PreInc _ (Var var _))  =
    [ Load var
    , Const (I 1)
    , VM.Add
    , Store var
    , Load var
    ]
  compile (UnaryOp PreInc p _)            = incrementVarError True False $ Just p
  compile (UnaryOp PostInc _ (Var var _)) =
    [ Load var
    , Load var
    , Const (I 1)
    , VM.Add
    , Store var
    ]
  compile (UnaryOp PostInc p _)           = incrementVarError False False $ Just p
  compile (UnaryOp op _ expr)             = compile expr ++ compile op
  compile (BinaryOp op _ e1 e2)           = compile e1 ++ compile e2 ++ compile op

instance Compilable UnaryOp where
  compile AST.Neg     = [VM.Neg]
  compile AST.Not     = [VM.Not]
  compile AST.PreDec  = incrementVarError True  True  Nothing
  compile AST.PostDec = incrementVarError False True  Nothing
  compile AST.PreInc  = incrementVarError True  False Nothing
  compile AST.PostInc = incrementVarError False False Nothing

instance Compilable BinaryOp where
  compile AST.Add     = [VM.Add]
  compile AST.Sub     = [VM.Sub]
  compile AST.Mul     = [VM.Mul]
  compile AST.Div     = [VM.Div]
  compile AST.Mod     = [VM.Mod]
  compile AST.Exp     = [VM.Exp]
  compile AST.Divides = [VM.Divides]
  compile AST.Eq      = [VM.Eq]
  compile AST.Ineq    = [VM.Ineq]
  compile AST.Lt      = [VM.Lt]
  compile AST.Gt      = [VM.Gt]
  compile AST.LtEq    = [VM.LtEq]
  compile AST.GtEq    = [VM.GtEq]
  compile AST.And     = [VM.And]
  compile AST.Or      = [VM.Or]

compileError :: String -> Maybe Source -> a
compileError msg p = error
  $ showString "Compile error"
  . maybe id
    ( \(Source start end) ->
      showString " ("
    . showString (sourceName start)
    . showString " "
    . shows (sourceLine start)
    . showString ":"
    . shows (sourceColumn start)
    . showString "-"
    . shows (sourceLine end)
    . showString ":"
    . shows (sourceColumn end)
    . showString ")"
    ) p
  . showString ": "
  . showString msg
  $ ""

incrementVarError :: Bool -> Bool -> Maybe Source -> a
incrementVarError pre dec = compileError $
  "Can only " ++ (if pre then "pre" else "post") ++ "-" ++
  (if dec then "dec" else "inc") ++ "rement variables"
