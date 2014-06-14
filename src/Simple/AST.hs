module Simple.AST where

import           Text.ParserCombinators.Parsec.Pos (SourcePos(..))

type Identifier = String

data Source
  = Source
  { start :: SourcePos
  , end   :: SourcePos
  } deriving (Show, Eq)

data Stmt
  = Seq    [Stmt]                            Source
  | While  Expr         Stmt                 Source
  | For    Stmt         Expr       Expr Stmt Source
  | IfElse Expr         Stmt       Stmt      Source
  | If     Expr         Stmt                 Source
  | Decl   Type         Identifier           Source
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
  | Void
  | Func
    deriving (Show, Eq)

data FuncType
  = FuncType
  { paramTypes :: [Type]
  , retType    :: Type
  } deriving (Show, Eq)

data FuncBody
  = ExprBody Expr
  | StmtBody Stmt
    deriving (Show, Eq)

class HasSource a where
  source :: a -> Source

instance HasSource Stmt where
  source (Seq stmts p)             = p
  source (While cond stmts p)      = p
  source (IfElse cond s1 s2 p)     = p
  source (If cond stmt p)          = p
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
