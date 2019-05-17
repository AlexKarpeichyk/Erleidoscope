module Lexer
{-
( Token (..)
, isID
, inRegex
, isString
, fixStrings
, isNum
, toInt
, specNot
, clearSpaces
, lex_
, tokenize
)
-}
 where

-- Necessary Imports

import Data.List.Split
import Data.Char
import Text.Regex.Posix

-- Datatypes

data Token = 
    Semicolon | LBracket | RBracket | LCurlyBracket Integer | RCurlyBracket Integer | Comma
  | EqualDefines | Equal | LessThan | GreaterThan | LessEqual | GreaterEqual 
  | Assign | Plus | Minus | Times | Divide
  | IDENTIFIER String | INTEGER Integer | BOOLEAN String | STRING String
  | Def | Skip | If | Then | Else | While | Do | Repeat | Until | Break | Continue | Print
  | TypeInt | TypeString | TypeBool | Fun [Token] | Bl [Token] deriving (Show, Eq)

-- Functions 

isID :: [Char] -> Bool
isID [] = False
isID [x]
  | isLower x = True
  | otherwise = False
isID (h:t)
  | isLower h && inRegex t = True
  | otherwise = False

inRegex :: String -> Bool
inRegex s = (s =~ "[a-z]*[A-Z]*[0-9]*_*" :: Bool)

isString :: String -> Bool
isString s
  | head s == '"' && last s == '"' = True
  | otherwise = False

fixStrings :: [String] -> [String]
fixStrings [] = []
fixStrings [x] = [x]
fixStrings (h:f:t)
  | head h == '"' && last h /=  '"' = fixStrings ((h ++ " " ++ f):t)
  | otherwise = h:(fixStrings (f:t)) 

isNum :: [Char] -> Bool
isNum [] = True
isNum [x] = isDigit x
isNum (h:t)
  | isDigit h = isNum t
  | otherwise = False

toInt :: String -> Integer
toInt s = read s :: Integer

specNot :: [String] -> [String]
specNot [] = []
specNot [x] = [x]
specNot (h:f:t)
  | h == "<" && f == "=" = "<=":specNot t
  | h == ">" && f == "=" = ">=":specNot t
  | h == "=" && f == "=" = "==":specNot t
  | h == ":" && f == "=" = ":=":specNot t
  | otherwise = h:(specNot (f:t))
 
clearSpaces :: [String] -> [String]
clearSpaces l = filter (\x -> x /= "" && x /= " " && x /= "\n" && x /= "\t") l

lex_ :: String -> [Token]
lex_ s = tokenize (specNot (fixStrings (clearSpaces (split (oneOf " \t\n(){}=+-/*><;,:") s)))) []

tokenize :: [String] -> [Token] -> [Token]
tokenize [] l = l
tokenize (h:t) l
  | h == ";" = tokenize (t) (l ++ [Semicolon])
  | h == "(" = tokenize (t) (l ++ [LBracket])
  | h == ")" = tokenize (t) (l ++ [RBracket])
  | h == "{" = tokenize (t) (l ++ [LCurlyBracket 0])
  | h == "}" = tokenize (t) (l ++ [RCurlyBracket 0])
  | h == "=" = tokenize (t) (l ++ [EqualDefines])
  | h == "==" = tokenize (t) (l ++ [Equal])
  | h == "<" = tokenize (t) (l ++ [LessThan])
  | h == ">" = tokenize (t) (l ++ [GreaterThan])
  | h == "<=" = tokenize (t) (l ++ [LessEqual])
  | h == ">=" = tokenize (t) (l ++ [GreaterEqual])
  | h == "," = tokenize (t) (l ++ [Comma])
  | h == ":=" = tokenize (t) (l ++ [Assign])
  | h == "+" = tokenize (t) (l ++ [Plus])
  | h == "-" = tokenize (t) (l ++ [Minus])
  | h == "*" = tokenize (t) (l ++ [Times])
  | h == "/" = tokenize (t) (l ++ [Divide])
  | h == "def" = tokenize (t) (l ++ [Def])
  | h == "skip" = tokenize (t) (l ++ [Skip])
  | h == "if" = tokenize (t) (l ++ [If])
  | h == "then" = tokenize (t) (l ++ [Then])
  | h == "else" = tokenize (t) (l ++ [Else])
  | h == "while" = tokenize (t) (l ++ [While])
  | h == "do" = tokenize (t) (l ++ [Do])
  | h == "repeat" = tokenize (t) (l ++ [Repeat])
  | h == "until" = tokenize (t) (l ++ [Until])
  | h == "break" = tokenize (t) (l ++ [Break])
  | h == "continue" = tokenize (t) (l ++ [Continue])
  | h == "int" = tokenize (t) (l ++ [TypeInt])
  | h == "string" = tokenize (t) (l ++ [TypeString])
  | h == "bool" = tokenize (t) (l ++ [TypeBool])
  | h == "true" = tokenize (t) (l ++ [(BOOLEAN "true")])
  | h == "false" = tokenize (t) (l ++ [(BOOLEAN "false")])
  | h == "print" = tokenize (t) (l ++ [Print])
  | (isNum h) = tokenize (t) (l ++ [INTEGER (toInt h)])
  | (isString h) = tokenize (t) (l ++ [STRING h])
  | (isID h) = tokenize (t) (l ++ [IDENTIFIER h])
  | otherwise = error ("Not in language: " ++ "\"" ++ h ++ "\"")
