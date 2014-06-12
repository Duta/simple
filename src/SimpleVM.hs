module SimpleVM where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)
import           Simple.Compiler (compile)
import           Simple.VM (execute)

main = do
  args <- getArgs
  unless (null args) $
    readFile (head args ++ ".simpc") >>= execute . read
