module Main where

import Lex (lexFile)

main = do
    filename <- getLine
    lexFile filename
