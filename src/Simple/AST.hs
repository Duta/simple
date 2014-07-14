module Simple.AST where

import           Text.ParserCombinators.Parsec.Pos (SourcePos(..))

type Identifier = String

data Source
  = Source
  { start :: SourcePos
  , end   :: SourcePos
  } deriving (Show, Eq)

data Type
  = Int
  | Bool
  | Void
  | Func
    deriving (Show, Eq)

data FuncType
  = FuncType
  { paramTypes  :: [Type]
  , funcRetType :: Type
  } deriving (Show, Eq)

class HasSource a where
  source :: a -> Source
