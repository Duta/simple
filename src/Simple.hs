module Simple where

import           Control.DeepSeq    (force)
import           Control.Monad      (unless)
import           System.Environment (getArgs)
import           Simple.Compiler    (compile)
import           Simple.FullAST     (reduceStmt)
import           Simple.Parser      (parseFile)
import           Simple.Typechecker (check)
import           Simple.VM          (execute)

main = do
  args <- getArgs
  unless (null args) $
    parseFile (head args ++ ".simp") >>= execute . force . compile . check . reduceStmt
