module Simple.Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

parseFile = undefined

languageDef :: LanguageDef st
languageDef = javaStyle
  { caseSensitive = True
  , reserved =
    -- Control structures
    [ "while"
    , "if"
    , "else"
    -- Primitive types
    , "int"
    , "bool"
    ]
  , reservedOps =
    -- Numerical
    [ "+"   -- Addition
    , "-"   -- Subtraction, negation
    , "*"   -- Multiplication
    , "/"   -- Division
    , "%"   -- Modulus
    , "^"   -- Exponentiation
    , "|"   -- 'Divides'
    -- Relational
    , "="   -- Equality
    , "=/=" -- Inequality
    , "<"   -- Lesser
    , ">"   -- Greater
    , "<="  -- Lesser or equal
    , ">="  -- Greater or equal
    , "¬"   -- 'Not'
    , "and" -- Conjunction
    , "or"  -- Disjunction
    -- Misc
    , ":="  -- Assignment
    ]
  }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace    = Token.whiteSpace    lexer

reservedOps = mapM reservedOp

simpleParser = undefined
