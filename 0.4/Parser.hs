module Parser
{-
( PROG (..)
, DEC (..)
, VARDEC (..)
, BLOCK (..)
, ENE (..)
, E (..)
, BLN (..)
, ARGS (..)
, COMP (..)
, BINOP (..)
, 
)-}
where

-- Necessary Imports

import Lexer

-- Datatypes

data PROG = Prog [DEC] deriving (Show)
data DEC = DEF (String) [PARAM] (BLOCK) deriving (Show)
data PARAM = Param (TYPE) (String) deriving (Show, Eq)
data TYPE = Int_ | String_ | Bool_ | Void_ deriving (Show, Eq)
data BLOCK = Block [E] deriving (Show, Eq)
data E = 
    ID (String)
  | STR (String)
  | INT (Integer)
  | BOOL (BLN)
  | BinOp (BINOP)
  | Comp (COMP)
  | COND (COMP) (BLOCK) (BLOCK)
  | Nested (BLOCK)
  | WHILE (COMP) (BLOCK)
  | ASSIGN (String) (E)
  | FUNCALL (String) [E]
  | SKIP
  | BREAK
  | CONTINUE
  | PRINT (E) deriving (Show, Eq)
data BLN = T | F deriving (Show, Eq)
data COMP = Eq (E) (E) | Less (E) (E) | Greater (E) (E) | LessEq (E) (E) | GreaterEq (E) (E) deriving (Show, Eq)
data BINOP = Add (E) (E) | Sub (E) (E) | Mult (E) (E) | Div (E) (E) deriving (Show, Eq)

-- Functions

-- 1. Parser pre-processing

isLBracket (LCurlyBracket x) = (True, x)
isLBracket _ = (False, -1)
isRBracket (RCurlyBracket x) = (True, x)
isRBracket _ = (False, -1)

getBlock x acc [] = acc
getBlock x acc (h:t)
  | not ((fst (isRBracket h)) && (snd (isRBracket h) == x)) = getBlock x (acc ++ [h]) t
  | otherwise = getBlock x acc []

getRest x [] = []
getRest x (h:t)
  | not ((fst (isRBracket h)) && (snd (isRBracket h) == x)) = getRest x t
  | otherwise = t

checkBrackets :: [Token] -> [Token] -> Bool
checkBrackets expec [] = null expec
checkBrackets expec (h:t)
  | not (elem h [LCurlyBracket 0, RCurlyBracket 0]) = checkBrackets expec t
  | h == LCurlyBracket 0 = checkBrackets ((RCurlyBracket 0):expec) t
  | h == RCurlyBracket 0 && length expec /= 0 = (h == head expec) && checkBrackets (tail expec) t
  | otherwise = False

labelBrackets expec acc [] = []
labelBrackets expec acc (h:t)
  | h == LCurlyBracket 0 = (LCurlyBracket (acc)):(labelBrackets (acc:expec) (acc + 1) t)
  | h == RCurlyBracket 0 = (RCurlyBracket (head expec)):(labelBrackets (tail expec) acc t)
  | otherwise = h:(labelBrackets expec acc t)

getFuns acc _ [] = []
getFuns acc False (Def:t) = getFuns [] True t
getFuns acc True ((Bl x):t) = (Fun (acc ++ [Bl x])):(getFuns [] False t)
getFuns acc True (h:t) = getFuns (acc ++ [h]) True t
getFuns acc False _ = error "Parse error: some syntax error."

preProc [] = []
preProc (h:t)
  | not (fst (isLBracket h)) = h:(preProc t)
  | fst (isLBracket h) = Bl (preProc (getBlock (snd (isLBracket h)) [] t)):(preProc (getRest (snd (isLBracket h)) t))
  | otherwise = error "Parse error: some syntax error."

preParse l
  | checkBrackets [] l = getFuns [] False (preProc (labelBrackets [] 1 l))
  | otherwise = error "Tokens preprocessing error: missmatched brackets in BLOCK formation."

-- 2. Main parser

parse (h:t) = parseProg (preParse (h:t))

parseProg :: [Token] -> PROG
parseProg ((Fun x):t) = Prog ((parseDec ((Fun x):t)))
parseProg _ = error "Parse error: not a program." 

parseDec :: [Token] -> [DEC]
parseDec [Fun x] = [DEF (getDefID x) (parseParams (fst (getParamsAndBlock [] (tail x)))) (parseBlock (snd (getParamsAndBlock [] (tail x))))]
parseDec ((Fun x):t) = (DEF (getDefID x) (parseParams (fst (getParamsAndBlock [] (tail x)))) (parseBlock (snd (getParamsAndBlock [] (tail x))))):(parseDec t)
parseDec _ = error "Parse error: error in function definition."

getDefID :: [Token] -> String
getDefID ((IDENTIFIER x):t) = x
getDefID _ = error "Parse error: error in function definition."

getParamsAndBlock :: [Token] -> [Token] -> ([Token], Token)
getParamsAndBlock [] [LBracket,RBracket,EqualDefines,(Bl y)] = ([], (Bl y))
getParamsAndBlock acc (LBracket:type_:(IDENTIFIER x):Comma:t) = getParamsAndBlock (acc ++ [type_, IDENTIFIER x]) (LBracket:t)
getParamsAndBlock acc [LBracket,type_,(IDENTIFIER x),RBracket,EqualDefines,(Bl y)] = ((acc ++ [type_, IDENTIFIER x]), (Bl y))
getParamsAndBlock acc _ = error "Parse error: error in variable declaration."

parseParams :: [Token] -> [PARAM]
parseParams [] = []
parseParams (type_:(IDENTIFIER x):t)
  | type_ == TypeInt = (Param (Int_) (x)):(parseParams t)
  | type_ == TypeString = (Param (String_) (x)):(parseParams t)
  | type_ == TypeBool = (Param (Bool_) (x)):(parseParams t)
  | otherwise = error "Parse error: unknown type."
parseParams _ = error "Parse error: error in variable declaration."

getBlock_ (h:f:t)
  | h /= EqualDefines = getBlock_ (f:t)
  | h == EqualDefines = f 

parseBlock :: Token -> BLOCK
parseBlock (Bl x) = Block (parseEXPS [] x)
parseBlock _ = error "Parse error: not a block."

parseEXPS :: [Token] -> [Token]-> [E]
parseEXPS acc [] = [parseE acc]
--parseEXPS [] [x] = [parseE [x]]
parseEXPS acc [x, Semicolon] = [parseE (acc ++ [x])]
parseEXPS acc (h:Semicolon:t) = (parseE (acc ++ [h])):(parseEXPS [] t)
parseEXPS acc (h:t) 
  | last (h:t) == Semicolon = parseEXPS (acc ++ [h]) t 
  | otherwise = error "Syntax error: ';' expected"

parseE :: [Token] -> E
parseE [BOOLEAN "true"] = BOOL T
parseE [BOOLEAN "false"] = BOOL F
parseE [IDENTIFIER s] = ID s
parseE [STRING s] = STR s
parseE [INTEGER x] = INT x
parseE [Skip] = SKIP
parseE [Bl x] = Nested (parseBlock (Bl x))
parseE [Break] = BREAK
parseE [Continue] = CONTINUE
parseE (Print:t) = PRINT (parsePrint [] t)
parseE ((IDENTIFIER s):Assign:t) = ASSIGN (s) (parseE t)
parseE [(IDENTIFIER s),LBracket,RBracket] = FUNCALL (s) []
parseE ((IDENTIFIER s):LBracket:t) = FUNCALL (s) (parseArgs [] (LBracket:t))
parseE (If:t) = parseCond [] (If:t)
parseE (While:t) = parseWhile [] (If:t)
parseE l
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = Comp (parseComp [] l) 
  | elem Plus l || elem Minus l = BinOp (parseBinop [] l)
  | elem Times l || elem Divide l = BinOp (parseBinop_ [] l)
  | otherwise = error "Parse error: unsupported token." 

parsePrint :: [Token] -> [Token] -> E
parsePrint acc [LBracket,RBracket] = parseE acc
parsePrint acc (LBracket:f:t) = parsePrint (acc ++ [f]) (LBracket:t)
parsePrint acc _ = error "Parse error: error in print statement."

parseArgs :: [Token] -> [Token] -> [E]
parseArgs acc [LBracket,x,RBracket] = [parseE (acc ++ [x])]
parseArgs acc (LBracket:x:Comma:t) = (parseE (acc ++ [x])):(parseArgs [] (LBracket:t))
parseArgs acc (LBracket:x:t) = parseArgs (acc ++ [x]) (LBracket:t)
parseArgs acc _ = error "Parse error: error in funciton call."

parseBinop :: [Token] -> [Token] -> BINOP
parseBinop acc [x]
  | elem x [Plus, Minus, Times, Divide] = error "Parse error: error in binary operation"
parseBinop acc (h:t) 
  | h == Plus = Add (parseE acc) (parseE t)
  | h == Minus = Sub (parseE acc) (parseE t)
  | otherwise = parseBinop (acc ++ [h]) t

parseBinop_ :: [Token] -> [Token] -> BINOP
parseBinop_ acc (h:t)
  | h == Times = Mult (parseE acc) (parseE t)
  | h == Divide = Div (parseE acc) (parseE t)
  | otherwise = parseBinop_ (acc ++ [h]) t

parseComp :: [Token] -> [Token] -> COMP
parseComp acc [x]
  | elem x [LessThan, GreaterThan, LessEqual, GreaterEqual, Equal] = error "Parse error: error in logical operation."
parseComp acc (h:t)
  | h == Equal = Eq (parseE acc) (parseE t)
  | h == LessThan = Less (parseE acc) (parseE t)
  | h == GreaterThan = Greater (parseE acc) (parseE t)
  | h == LessEqual = LessEq (parseE acc) (parseE t)
  | h == GreaterEqual = GreaterEq (parseE acc) (parseE t)
  | otherwise = parseComp (acc ++ [h]) t 

parseCond :: [Token] -> [Token] -> E
parseCond acc [If,x,Then,(Bl y),Else,(Bl z)]
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = COND (parseComp [] (acc ++ [x])) (parseBlock (Bl y)) (parseBlock (Bl z))
  | otherwise = error "Parse error: error in conditional."
  where 
    l = (acc ++ [x])
parseCond acc (If:x:t) = parseCond (acc ++ [x]) (If:t)
parseCond acc _ = error "Parse error: error in conditional."

parseWhile :: [Token] -> [Token] -> E
parseWhile acc [While,x,Do,(Bl y)]
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = WHILE (parseComp [] (acc ++ [x])) (parseBlock (Bl y))
  | otherwise = error "Parse error: error in while-lopp."
  where
    l = (acc ++ [x])
parseWhile acc (If:x:t) = parseWhile (acc ++ [x]) (While:t)
parseWhile acc _ = error "Parse error: error in while-lopp."
