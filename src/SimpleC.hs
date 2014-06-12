module SimpleC where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)
import           Simple.Compiler (compile)
import           Simple.VM (execute)

main = do
  args <- getArgs
  unless (null args) $
    let file = head args in
    parseFile (file ++ ".simp") >>=
    writeFile (file ++ ".simpc") . show . compile
