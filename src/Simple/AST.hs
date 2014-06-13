module Simple.AST where

import           Text.ParserCombinators.Parsec.Pos (SourcePos(..))

data Source = Source {start :: SourcePos, end :: SourcePos} deriving (Show, Eq)

data Stmt
  = Seq    [Stmt]                 Source
  | While  Expr   Stmt            Source
  | IfElse Expr   Stmt       Stmt Source
  | If     Expr   Stmt            Source
  | Init   Type   Identifier Expr Source
  | Expr   Expr                   Source
    deriving (Show, Eq)

data Expr
  = Set      Identifier Expr        Source
  | FuncCall Identifier [Expr]      Source
  | Var      Identifier             Source
  | IntLit   Int                    Source
  | BoolLit  Bool                   Source
  | UnaryOp  UnaryOp    Source Expr
  | BinaryOp BinaryOp   Source Expr Expr
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
