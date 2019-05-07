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
            BoolValue |
            NullValue | 
            Keyword |
            StringValue deriving (Eq, Show)

data Lexeme = Lexeme {
    loc :: (Int, Int), 
    tok :: Token,
    val :: String } deriving Show

type Handled = (Lexeme, LexerState, String)

startState = LexerState {
    currLoc = (0,0),
    currString = "",
    lastString = "",
    lastToken = Start}

startLexeme = Lexeme {
    loc = (0,0),
    tok = Start,
    val = "<START>"}

punctuators = "!$():=@[]{}|"
keywords = ["query", "mutation", "keyword", "subscription", "schema", "extend",
            "on", "scalar", "implements", "type", "interface", "union", "enum",
            "input", "directive"] 

lexProgram :: String -> [Lexeme]
lexProgram program = getLexeme startState program [startLexeme]

getLexeme :: LexerState -> String -> [Lexeme] -> [Lexeme]
getLexeme s [] lexs = lexs ++ [Lexeme (currLoc s) End "<END>"]
getLexeme s ('\xFEFF':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s (',':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s ('#':rest) lexs = getLexeme (moveNewLine s) (skipComment rest s) lexs
getLexeme s ('.':'.':'.':rest) lexs = undefined
getLexeme s (char:rest) lexs
    | isSpace char = getLexeme (moveChar s) rest lexs
    | char `elem` punctuators =
        let (handledLex, handledState, handledRest) = handlePunct rest (addChar s char) in
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | isLetter char || char == '_' = 
        let (handledLex, handledState, handledRest) = handleName rest (addChar s char) in 
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | isNumber char || char == '-' =
        let (handledLex, handledState, handledRest) = handleNumber rest (addChar s char) in 
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | char == '+' =
        -- TODO: add state for +123e456
        let (handledLex, handledState, handledRest) = handleFloat rest (addChar s char) in 
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | char == '"' =
        let (handledLex, handledState, handledRest) = handleString rest s in 
        getLexeme handledState handledRest (lexs ++ [handledLex])
    | otherwise = getLexeme (moveChar s) rest lexs

skipComment :: String -> LexerState -> String
skipComment ('\x000A':rest) s = rest
skipComment ('\x000D':rest) s = rest
skipComment (char:rest) s = skipComment rest s
skipComment [] s = ""

handleName :: String -> LexerState -> Handled
handleName (char:rest) state
    | char == '_' = handleName rest (addChar state '_')
    | isLetter char || isNumber char = 
        handleName rest (addChar state char)
handleName rest state
    | currStr == "true" || currStr == "false" =
        handledFactory state rest BoolValue
    | currStr == "null" =
        handledFactory state rest NullValue
    | currStr `elem` keywords = handledFactory state rest Keyword
    | otherwise = handledFactory state rest Name
    where currStr = currString state

handlePunct :: String -> LexerState -> Handled
handlePunct rest state = handledFactory state rest Punctuator

handleNumber :: String -> LexerState -> Handled
handleNumber (char:rest) state
    | isNumber char = handleNumber rest (addChar state char)
    | char == '.' || char == 'e' || char == 'E' =
        handleFloat rest (addChar state char)
handleNumber rest state = if currString state == "-" then
                            lexerError state "Illegal '-'" 
                            else 
                            handledFactory state rest IntValue

handleFloat :: String -> LexerState -> Handled
handleFloat (char:rest) state
    | isNumber char = handleFloat rest (addChar state char)
handleFloat rest state =
        handledFactory state rest FloatValue

handleString :: String -> LexerState -> Handled
handleString ('"':rest) state = handledFactory state rest StringValue
handleString (char:rest) state = handleString rest (addChar state char)
handleString [] state = lexerError state "Reached EOF in string"

handledFactory :: LexerState -> String -> Token -> Handled
handledFactory state rest tokType = (Lexeme { 
                        loc = (currLoc state),
                        tok = tokType,
                        val = (currString state)},
                      state {
                          lastToken = tokType,
                          lastString = currString state,
                          currString = ""},
                      rest)

lexerError :: LexerState -> String -> a
lexerError state string = error $ "LEX ERROR " ++ (show (currLoc state)) ++ ": " ++ string
