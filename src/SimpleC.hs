module SimpleC where

import           Control.Monad (unless)
import           Simple.Compiler (compile)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)
import           Simple.Typechecker (check)
import           Simple.VM (execute)

main = do
  args <- getArgs
  unless (null args) $
    let file = head args in
    parseFile (file ++ ".simp") >>=
    writeFile (file ++ ".simpc") . show . compile . check
