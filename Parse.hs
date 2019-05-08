module Parse (parseProgram)  where

import Lex

data AST = Leaf | Node [AST] deriving Show

parseProgram :: [Lexeme] -> AST
parseProgram = undefined
