module Main where

import Simple.Parser (parseFile)
import Simple.Compiler (compile)
import System.Environment (getArgs)

main = getArgs >>= (>>= print . compile) . parseFile
