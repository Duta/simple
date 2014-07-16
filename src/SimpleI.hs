module SimpleI where

import           Control.DeepSeq    (force)
import           Control.Monad      (unless)
import           System.Environment (getArgs)
import           Simple.FullAST     (reduceStmt)
import           Simple.Interpreter (run)
import           Simple.Parser      (parseFile)
import           Simple.Typechecker (check)

main = do
  args <- getArgs
  unless (null args) $
    parseFile (head args ++ ".simp") >>= run . check . reduceStmt
