module Main where

import Simple.Parser (parseFile)
import Simple.Compiler (compile)
import Simple.VM (execute)
import System.Environment (getArgs)

main = getArgs >>= mapM_ ((>>= execute . compile) . parseFile)
