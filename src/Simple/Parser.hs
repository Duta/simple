module Simple.Parser where

import           Control.Monad (liftM)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import           Simple.AST

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse simpleParser "" program of
   Left e  -> print e >> fail "parse error"
   Right r -> return r

languageDef :: LanguageDef st
languageDef = javaStyle
  { caseSensitive = True
  , opStart       = opLetter languageDef
  , opLetter      = oneOf "+-*/%^|=<>¬andor:"
  , reservedNames =
    -- Control structures
    [ "while"
    , "if"
    , "else"
    -- Primitive types
    , "int"
    , "bool"
    ]
  , reservedOpNames =
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

reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
braces        = Token.braces        lexer
commaSep      = Token.commaSep      lexer
identifier    = Token.identifier    lexer
integer       = Token.integer       lexer
stringLiteral = Token.stringLiteral lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer

reservedOps = mapM reservedOp

simpleParser :: Parser Stmt
simpleParser = do
  whiteSpace
  stmts <- statements
  eof
  return stmts

statement :: Parser Stmt
statement = parens statement
        <|> whileStatement
        <|> try ifElseStatement
        <|> ifStatement
        <|> initStatement
        <|> basicStatement
        <|> bracedStatements
        <?> "statement"

whileStatement :: Parser Stmt
whileStatement = do
  reserved "while"
  cond <- expression
  stmts <- bracedStatements
  return $ While cond stmts

ifElseStatement :: Parser Stmt
ifElseStatement = do
  If cond stmts1 <- ifStatement
  reserved "else"
  stmts2 <- bracedStatements
  return $ IfElse cond stmts1 stmts2

ifStatement :: Parser Stmt
ifStatement = do
  reserved "if"
  cond <- expression
  stmts <- bracedStatements
  return $ If cond stmts

initStatement :: Parser Stmt
initStatement = do
  varType <- typeName
  Set var expr <- assignment
  semi
  return $ Init varType var expr

basicStatement :: Parser Stmt
basicStatement = do
  expr <- expression
  semi
  return $ Expr expr

bracedStatements :: Parser Stmt
bracedStatements = braces statements

statements :: Parser Stmt
statements = liftM Seq $ many statement

expression :: Parser Expr
expression = buildExpressionParser operators terminals
         <?> "expression"

operators :: OperatorTable Char st Expr
operators =
  [ [Prefix (reservedOp "-"   >> return (UnaryOp Neg))]
  , [Prefix (reservedOp "¬"   >> return (UnaryOp Not))]
  , [Infix  (reservedOp "^"   >> return (BinaryOp Exp))     AssocLeft]
  , [Infix  (reservedOp "*"   >> return (BinaryOp Mul))     AssocLeft]
  , [Infix  (reservedOp "/"   >> return (BinaryOp Div))     AssocLeft]
  , [Infix  (reservedOp "%"   >> return (BinaryOp Mod))     AssocLeft]
  , [Infix  (reservedOp "|"   >> return (BinaryOp Divides)) AssocLeft]
  , [Infix  (reservedOp "+"   >> return (BinaryOp Add))     AssocLeft]
  , [Infix  (reservedOp "-"   >> return (BinaryOp Sub))     AssocLeft]
  , [Infix  (reservedOp "="   >> return (BinaryOp Eq))      AssocLeft]
  , [Infix  (reservedOp "=/=" >> return (BinaryOp Ineq))    AssocLeft]
  , [Infix  (reservedOp "<"   >> return (BinaryOp Lt))      AssocLeft]
  , [Infix  (reservedOp ">"   >> return (BinaryOp Gt))      AssocLeft]
  , [Infix  (reservedOp "<="  >> return (BinaryOp LtEq))    AssocLeft]
  , [Infix  (reservedOp ">="  >> return (BinaryOp GtEq))    AssocLeft]
  , [Infix  (reservedOp "and" >> return (BinaryOp And))     AssocLeft]
  , [Infix  (reservedOp "or"  >> return (BinaryOp Or))      AssocLeft]
  ]

terminals :: Parser Expr
terminals = parens expression
        <|> try functionCall
        <|> try assignment
        <|> liftM Var identifier
        <|> liftM (IntLit . fromIntegral) integer
        <|> liftM BoolLit boolean
        <?> "terminal expression"

assignment :: Parser Expr
assignment = do
  var <- identifier
  reservedOp ":="
  expr <- expression
  return $ Set var expr

functionCall :: Parser Expr
functionCall = do
  func <- identifier
  args <- parens $ commaSep expression
  return $ FuncCall func args

typeName :: Parser Type
typeName = (reserved "int"  >> return Int)
       <|> (reserved "bool" >> return Bool)
       <?> "type name"

boolean :: Parser Bool
boolean = (reserved "true"  >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean literal"
