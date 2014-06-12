module Simple.AST where

data Stmt
  = Seq [Stmt]
  | While Expr Stmt
  | IfElse Expr Stmt Stmt
  | If Expr Stmt
  | Init Type Identifier Expr
  | Expr Expr
    deriving (Show, Eq)

data Expr
  = Set Identifier Expr
  | FuncCall Identifier [Expr]
  | Var Identifier
  | IntLit Int
  | BoolLit Bool
  | UnaryOp UnaryOp Expr
  | BinaryOp BinaryOp Expr Expr
    deriving (Show, Eq)

data UnaryOp
  = Neg
  | Not
    deriving (Show, Eq)

data BinaryOp
  = Add
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
    deriving (Show, Eq)

data Type
  = Int
  | Bool
    deriving (Show, Eq)

type Identifier = String
