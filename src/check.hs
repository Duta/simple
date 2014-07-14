module Check where

import           Control.Monad (unless)
import           Simple.Parser (parseFile)
import           Simple.Compiler (compile)
import           Simple.Typechecker (check)
import           System.Environment (getArgs)

main = do
  args <- getArgs
  unless (null args) $
    let file = head args in
    parseFile (file ++ ".simp") >>=
    writeFile (file ++ ".simpd") . show . check
