module Parse where

import Lex
import Data.Sequence

data ParseState = ParseState {
        tokens :: Seq Lexeme,
        ast :: Seq Definition
        }

data Definition = Definition Value deriving Show

data Value = NameVal Lexeme |
            IntVal Lexeme |
            FloatVal Lexeme |
            BoolVal Lexeme |
            StringVal Lexeme |
            NullVal Lexeme |
            EnumVal Lexeme |
            ListVal [Lexeme] deriving Show

parse :: Seq Lexeme -> ParseState
parse lexs = parseDocument $ ParseState { tokens = lexs, ast = empty }

parseDocument :: ParseState -> ParseState
parseDocument ps = 
    | tokenType == Name = ps {
        where 
            thisToken = index 0 $ tokens ps in
            tokenType = tok thisToken
            tokenVal = val thisToken
