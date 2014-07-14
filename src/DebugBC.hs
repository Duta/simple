module DebugBC where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           Simple.FullAST (reduceStmt)
import           Simple.Parser (parseFile)
import           Simple.Compiler (compile)

main = do
  args <- getArgs
  unless (null args) $
    let file = head args in
    parseFile (file ++ ".simp") >>=
    writeFile (file ++ ".simpd") . show . compile . reduceStmt
