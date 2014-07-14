module Simple.FullAST where

import           Control.Applicative ((<$>))
import           Simple.AST
import           Simple.Primitives   (defaultExpr)
import qualified Simple.ReducedAST   as R

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

data FuncBody
  = ExprBody Expr
  | StmtBody Stmt
    deriving (Show, Eq)

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

reduceStmt :: Stmt -> R.Stmt
reduceStmt (Seq    stmts                        p) = R.Seq    (map reduceStmt stmts) p
reduceStmt (While  cond     stmts               p) = R.While  (reduceExpr cond) (reduceStmt stmts) p
reduceStmt (For    ini      cond  inc  stmts    p) = R.For    (reduceStmt ini) (reduceExpr cond) (reduceExpr inc) (reduceStmt stmts) p
reduceStmt (IfElse cond     s1    s2            p) = R.IfElse (reduceExpr cond) (reduceStmt s1) (reduceStmt s2) p
reduceStmt (If     cond     stmt                p) = R.IfElse (reduceExpr cond) (reduceStmt stmt) (R.Seq [] p) p
reduceStmt (Decl   varType  var                 p) = R.Init   varType var (defaultExpr varType) p
reduceStmt (Init   varType  var   expr          p) = R.Init   varType var (reduceExpr expr) p
reduceStmt (Expr   expr                         p) = R.Expr   (reduceExpr expr) p
reduceStmt (Return expr                         p) = R.Return (reduceExpr <$> expr) p

reduceExpr :: Expr -> R.Expr
reduceExpr (Set      var      expr        p) = R.Set      var (reduceExpr expr) p
reduceExpr (FuncCall func     args        p) = R.FuncCall func (map reduceExpr args) p
reduceExpr (Lambda   funcType params body p) = R.Lambda   funcType params (reduceFuncBody body) p
reduceExpr (Var      var                  p) = R.Var      var p
reduceExpr (IntLit   int                  p) = R.IntLit   int p
reduceExpr (BoolLit  bool                 p) = R.BoolLit  bool p
reduceExpr (UnaryOp  Neg     p expr)       = R.UnaryOp  R.Neg     p (reduceExpr expr)
reduceExpr (UnaryOp  Not     p expr)       = R.UnaryOp  R.Not     p (reduceExpr expr)
reduceExpr (UnaryOp  PreDec  p expr)       = R.UnaryOp  R.PreDec  p (reduceExpr expr)
reduceExpr (UnaryOp  PostDec p expr)       = R.UnaryOp  R.PostDec p (reduceExpr expr)
reduceExpr (UnaryOp  PreInc  p expr)       = R.UnaryOp  R.PreInc  p (reduceExpr expr)
reduceExpr (UnaryOp  PostInc p expr)       = R.UnaryOp  R.PostInc p (reduceExpr expr)
reduceExpr (BinaryOp Add     p e1 e2)      = R.BinaryOp R.Add     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Sub     p e1 e2)      = R.BinaryOp R.Sub     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Mul     p e1 e2)      = R.BinaryOp R.Mul     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Div     p e1 e2)      = R.BinaryOp R.Div     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Mod     p e1 e2)      = R.BinaryOp R.Mod     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Exp     p e1 e2)      = R.BinaryOp R.Exp     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Divides p e1 e2)      = R.BinaryOp R.Eq p (R.BinaryOp R.Mod p (reduceExpr e2) (reduceExpr e1)) (R.IntLit 0 p)
reduceExpr (BinaryOp Eq      p e1 e2)      = R.BinaryOp R.Eq      p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Ineq    p e1 e2)      = R.BinaryOp R.Ineq    p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Lt      p e1 e2)      = R.BinaryOp R.Lt      p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Gt      p e1 e2)      = R.BinaryOp R.Gt      p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp LtEq    p e1 e2)      = R.BinaryOp R.LtEq    p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp GtEq    p e1 e2)      = R.BinaryOp R.GtEq    p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp And     p e1 e2)      = R.BinaryOp R.And     p (reduceExpr e1) (reduceExpr e2)
reduceExpr (BinaryOp Or      p e1 e2)      = R.BinaryOp R.Or      p (reduceExpr e1) (reduceExpr e2)

reduceFuncBody :: FuncBody -> R.FuncBody
reduceFuncBody (ExprBody expr) = R.ExprBody $ reduceExpr expr
reduceFuncBody (StmtBody stmt) = R.StmtBody $ reduceStmt stmt
