module Lexer
where

-- Necessary Imports

import Data.Map
import Data.Char
import Data.List.Split
import Text.Regex.Posix

-- Datatypes

data Token = 
   Semicolon | LBracket | RBracket | LCurlyBracket | RCurlyBracket | Comma |
   EqualDefines | Equal | LessThan | GreaterThan | LessEqual | GreaterEqual | NotEqual |
   Assign | Plus | Minus | Times | Divide |
   IDENTIFIER String | INTEGER Integer | BOOLEAN String | STRING String |
   Def | Skip | If | Then | Else | While | Do | Break | Continue | Print | Spawn deriving (Show, Eq, Ord)

-- Globals

tokens :: Map String Token
tokens = fromList [(";", Semicolon), ("(", LBracket), (")", RBracket), ("{", LCurlyBracket), ("}", RCurlyBracket), (",", Comma), ("=", EqualDefines), ("==", Equal), ("<", LessThan), (">", GreaterThan), ("<=", LessEqual), (">=", GreaterEqual), (":=", Assign), ("!=", NotEqual), ("+", Plus), ("-", Minus), ("*", Times), ("/", Divide), ("def", Def), ("skip", Skip), ("if", If), ("then", Then), ("else", Else), ("while", While), ("do", Do), ("break", Break), ("continue", Continue), ("print", Print), ("spawn", Spawn)]

-- Functions

isNum :: [Char] -> Bool
isNum [x] = isDigit x
isNum (h:t) = (isDigit h) && (isNum t)

isID :: String -> Bool
isID [x] = isLower x
isID (h:t) = (isLower h) && ((h:t) =~ "[a-z]*[A-Z]*[0-9]*_*" :: Bool)

isBool :: String -> Bool
isBool "true" = True
isBool "false" = True
isBool _ = False

isString :: String -> Bool
isString s = last s == '"' && head s == '"'

token :: String -> Token
token s
  | member s tokens = cleanToken (Data.Map.lookup s tokens)
  | isNum s = INTEGER (read s :: Integer)
  | isBool s = BOOLEAN s
  | isID s = IDENTIFIER s
  | isString s = STRING s
  | otherwise = error ("Lexical error: unknown token (" ++ s ++ ")")

cleanToken (Just t) = t

lexString :: [String] -> (String, [String])
lexString ("\"":x:"\"":t) = ("\"" ++ x ++ "\"", t)
lexString ("\"":x:y:t) = lexString ("\"":(x ++ y):t)
lexString ["\"",x] = error ("Lexical error: unclosed string literal (" ++ "\"" ++ l ++ " ..." ++ ")")
  where l = if length x > 10 then take 10 x else x

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize [x]
  | elem x [" ", "", "\n", "\t"] = []
  | otherwise = [token x]
tokenize (h:f:t)
  | h == "\"" = [token (fst string)] ++ (tokenize (snd string))
  | elem h specs && elem f spaces = tokenize (h:t)
  | elem h specs && f == "=" = [token (h ++ f)] ++ (tokenize t)
  | elem h spaces = tokenize (f:t)
  | otherwise = [token h] ++ (tokenize (f:t))
    where string = lexString (h:f:t)
          specs = ["=", ":", "!", ">", "<"]
          spaces = [" ", "", "\n", "\t"]

lex_ :: String -> [Token]
lex_ s = tokenize (Data.List.Split.split (oneOf " \t\n(){}=+-/*><;,:\"") s)