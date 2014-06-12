module Main where

import Simple.Parser (parseFile)
import Simple.Compiler (compile)
import System.Environment (getArgs)

main = getArgs >>= mapM_ ((>>= print . compile) . parseFile)
