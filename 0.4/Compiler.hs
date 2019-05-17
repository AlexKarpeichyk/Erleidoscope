
-- Necessary imports 

import Lexer 
import Parser
import Codegen

-- Compiling from string 

compile out s = writeFile ("output/" ++ out ++ ".ll") (codegen (parse (lex_ s)))

-- Compiling from file


