module Simple.ReducedAST where

import           Simple.AST

data Stmt
  = Seq    [Stmt]                            Source
  | While  Expr         Stmt                 Source
  | For    Stmt         Expr       Expr Stmt Source
  | IfElse Expr         Stmt       Stmt      Source
  | Init   Type         Identifier Expr      Source
  | Expr   Expr                              Source
  | Return (Maybe Expr)                      Source
    deriving (Show, Eq)

data Expr
  = Set      Identifier Expr                  Source
  | FuncCall Identifier [Expr]                Source
  | Lambda   FuncType   [Identifier] FuncBody Source
  | Var      Identifier                       Source
  | IntLit   Int                              Source
  | BoolLit  Bool                             Source
  | UnaryOp  UnaryOp  Source Expr
  | BinaryOp BinaryOp Source Expr Expr
    deriving (Show, Eq)

data UnaryOp
  = Neg
  | Not
  | PreDec
  | PostDec
  | PreInc
  | PostInc
    deriving (Show, Eq)

data BinaryOp
  = Add
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
    deriving (Show, Eq)

data FuncBody
  = ExprBody Expr
  | StmtBody Stmt
    deriving (Show, Eq)

instance HasSource Stmt where
  source (Seq stmts p)             = p
  source (While cond stmts p)      = p
  source (IfElse cond s1 s2 p)     = p
  source (Init varType var expr p) = p
  source (Expr expr p)             = p

instance HasSource Expr where
  source (Set var expr p)       = p
  source (FuncCall func args p) = p
  source (Var var p)            = p
  source (IntLit int p)         = p
  source (BoolLit bool p)       = p
  source (UnaryOp op p expr)    = p
  source (BinaryOp op p e1 e2)  = p
