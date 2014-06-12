module Simple where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)
import           Simple.Compiler (compile)
import           Simple.VM (execute)

main = do
  args <- getArgs
  unless (null args) $
    parseFile (head args ++ ".simp") >>= execute . compile
