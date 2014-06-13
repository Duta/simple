module Simple where

import           Control.DeepSeq (force)
import           Control.Monad (unless)
import           Simple.Compiler (compile)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)
import           Simple.Typechecker (check)
import           Simple.VM (execute)

main = do
  args <- getArgs
  unless (null args) $
    parseFile (head args ++ ".simp") >>= execute . force . compile . check
