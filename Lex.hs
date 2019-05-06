module Lex where

import Data.Char

data LexerState = LexerState {
        currLoc :: (Int, Int),
        currString :: String,
        lastString :: String,
        lastToken :: Token }

addChar :: LexerState -> Char -> LexerState
addChar s c = (moveChar s) { 
    currString = string ++ [c]
    }
    where string = currString s

moveChar :: LexerState -> LexerState
moveChar s = s {
    currLoc = (fst oldLoc, (snd oldLoc)+1)
    }
    where oldLoc = currLoc s

moveNewLine :: LexerState -> LexerState
moveNewLine s = s {
    currLoc = ((fst oldLoc) + 1, 0)
    }
    where oldLoc = currLoc s

data Token = Start |
            End |
            Punctuator |
            Name |
            IntValue |
            FloatValue |
            StringValue deriving (Eq, Show)

data Lexeme = Lexeme {
    loc :: (Int, Int), 
    tok :: Token,
    str :: String } deriving Show

type Handled = (Lexeme, LexerState, String)

startState = LexerState {
    currLoc = (0,0),
    currString = "",
    lastString = "",
    lastToken = Start
}

punctuators = "!$():=@[]{}|"

lexProgram :: String -> [Lexeme]
lexProgram program = getLexeme startState program []

getLexeme :: LexerState -> String -> [Lexeme] -> [Lexeme]
getLexeme s [] lexs = lexs ++ [Lexeme (currLoc s) End "<END>"]
getLexeme s ('\xFEFF':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s (',':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s ('#':rest) lexs = getLexeme (moveNewLine s) (skipComment rest) lexs
getLexeme s ('.':'.':'.':rest) lexs = undefined
getLexeme s (char:rest) lexs
    | isSpace char = getLexeme (moveChar s) rest lexs
    | char `elem` punctuators =
        let (handledLex, handledState, handledRest) = handlePunct rest (addChar s char) in
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | isLetter char || char == '_' = 
        let (handledLex, handledState, handledRest) = handleName rest (addChar s char) in 
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | otherwise = getLexeme (moveChar s) rest lexs

skipComment :: String -> String
skipComment ('\x000A':rest) = rest
skipComment ('\x000D':rest) = rest
skipComment (char:rest) = skipComment rest

handleName :: String -> LexerState -> Handled
handleName (char:rest) state
    | char == '_' = handleName rest (addChar state '_')
    | isLetter char || isNumber char = handleName rest (addChar state char)
handleName rest state = handledFactory state rest

handlePunct :: String -> LexerState -> Handled
handlePunct rest state = handledFactory state rest

handledFactory :: LexerState -> String -> Handled
handledFactory state rest = (Lexeme { 
                        loc = (currLoc state),
                        tok = Punctuator,
                        str = (currString state)},
                      state {
                          lastString = currString state,
                          currString = ""},
                      rest)