module DebugAST where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           Simple.Parser (parseFile)

main = do
  args <- getArgs
  unless (null args) $
    let file = head args in
    parseFile (file ++ ".simp") >>=
    writeFile (file ++ ".simpd") . show
