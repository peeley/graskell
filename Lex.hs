module Lex (lexString, Lexeme) where

import Data.Char
import Data.Sequence

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
    currString = (currString s) ++ "\n",
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

lexString :: String -> Seq Lexeme
lexString program = getLexeme startState program $ singleton startLexeme

lexFile :: String -> IO ()
lexFile filename = readFile filename >>= sequence_ . fmap (putStrLn . show) . lexString

getLexeme :: LexerState -> String -> Seq Lexeme -> Seq Lexeme
getLexeme s [] lexs = lexs |> Lexeme (currLoc s) End "<END>"
getLexeme s ('\xFEFF':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s (',':rest) lexs = getLexeme (moveChar s) rest lexs
getLexeme s ('#':rest) lexs = getLexeme (moveNewLine s) (skipComment rest s) lexs
getLexeme s ('.':'.':'.':rest) lexs = 
    let newState = (moveChar . moveChar . moveChar) s in
    getLexeme newState rest $ lexs |> Lexeme (currLoc s) Punctuator "..."
getLexeme s ('"':'"':'"':rest) lexs =
    let newState = (moveChar . moveChar . moveChar) s in
    let (hLex, hState, hRest) = handleBlock rest newState in
    getLexeme hState hRest (lexs |> hLex)
getLexeme s (char:rest) lexs
    | isSpace char = getLexeme (moveChar s) rest lexs
    | char `elem` punctuators =
        let (hLex, hState, hRest) = handlePunct rest (addChar s char) in
        getLexeme hState hRest (lexs |> hLex)
    | isLetter char || char == '_' = 
        let (hLex, hState, hRest) = handleName rest (addChar s char) in 
        getLexeme hState hRest (lexs |> hLex)
    | isNumber char || char == '-' =
        let (hLex, hState, hRest) = handleNumber rest (addChar s char) in 
        getLexeme hState hRest (lexs |> hLex)
    | char == '"' =
        let (hLex, hState, hRest) = handleString rest s in 
        getLexeme hState hRest (lexs |> hLex)
    | otherwise = lexerError s $ "Illegal character " ++ [char]

skipComment :: String -> LexerState -> String
skipComment ('\x000A':rest) s = rest
skipComment ('\x000D':rest) s = rest
skipComment (char:rest) s = skipComment rest s
skipComment [] s = ""

handleName :: String -> LexerState -> Handled
handleName [] state = handledFactory state "" Name
handleName str@(char:rest) state
    | char == '_' = handleName rest (addChar state '_')
    | isLetter char || isNumber char = 
        handleName rest (addChar state char)
    | currStr == "true" || currStr == "false" =
        handledFactory state str BoolValue
    | currStr == "null" =
        handledFactory state str NullValue
    | currStr `elem` keywords = handledFactory state str Keyword
    | otherwise = handledFactory state str Name
    where currStr = currString state

handlePunct :: String -> LexerState -> Handled
handlePunct rest state = handledFactory state rest Punctuator

handleNumber :: String -> LexerState -> Handled
handleNumber [] state = handledFactory state "" IntValue
handleNumber str@(char:rest) state
    | isNumber char = handleNumber rest $ addChar state char
    | char == '.'  = 
        handleFloat rest (addChar state char)
    | char == 'e' || char == 'E' =
        let (nextChar:remaining) = rest in
        let expState = addChar state char in
        if nextChar == '+' || nextChar == '-' then
            handleExp remaining $ addChar expState nextChar
        else
            handleExp remaining $ addChar (addChar expState '+') nextChar
    | otherwise = handledFactory state rest IntValue

handleFloat :: String -> LexerState -> Handled
handleFloat [] state = handledFactory state "" FloatValue
handleFloat str@(char:rest) state
    | isNumber char = 
        handleFloat rest (addChar state char)
    | char == 'e' || char == 'E' =
        handleExp rest (addChar state char)
    | otherwise = handledFactory state str FloatValue

handleExp :: String -> LexerState -> Handled
handleExp [] state = handledFactory state "" FloatValue
handleExp str@(char:rest) state
    | isNumber char = handleExp rest (addChar state char)
    | otherwise = handledFactory state str FloatValue

handleString :: String -> LexerState -> Handled
handleString ('"':rest) state = handledFactory state rest StringValue
handleString ('\\':'u':char1:char2:char3:char4:rest) state = 
    let uniBytes = [char1, char2 ,char3, char4] in
    if all isHexDigit uniBytes then
        handleString rest $ addChar state $ (fst . head . readLitChar) $ "\\x" ++ uniBytes
    else
        handleString rest $ foldl addChar state uniBytes
handleString ('\\':char:rest) state = handleString rest (addChar state char)
handleString ('\n':_) state = 
    lexerError state "Illegal line terminator in non-block string"
handleString (char:rest) state = handleString rest (addChar state char)
handleString [] state = lexerError state "Reached EOF in string"

handleBlock :: String -> LexerState -> Handled
handleBlock ('"':'"':'"':rest) state = handledFactory state rest StringValue
handleBlock ('\\':'"':'"':'"':rest) state = 
    handleBlock rest (foldl addChar state "\"\"\"")
handleBlock ('\n':rest) state = handleBlock rest (moveNewLine state)
handleBlock (char:rest) state = handleBlock rest (addChar state char)

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
lexerError state string = error $ "Invalid Query " ++ (show (currLoc state)) ++ ": " ++ string
