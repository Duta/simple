module Simple.Parser (parseFile) where

import           Control.Monad (liftM)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
import           Simple.AST
import           Simple.FullAST

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse simpleParser file program of
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
    , "for"
    , "if"
    , "else"
    -- Primitive types
    , "int"
    , "bool"
    -- Primitive values
    , "true"
    , "false"
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
    , "++"  -- Pre/post-increment
    , "--"  -- Pre/post-decrement
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
    , "->"  -- Lambda
    , ":"   -- 'Returns'
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

annotate :: Parser (Source -> a) -> Parser a
annotate p = do
  start <- getPosition
  f <- p
  end <- getPosition
  return . f $ Source start end

statement :: Parser Stmt
statement = parens statement
        <|> whileStatement
        <|> forStatement
        <|> try ifElseStatement
        <|> ifStatement
        <|> returnStatement
        <|> try initStatement
        <|> declStatement
        <|> basicStatement
        <|> bracedStatements
        <?> "statement"

whileStatement :: Parser Stmt
whileStatement = annotate $ do
  reserved "while"
  cond <- expression
  stmts <- bracedStatements
  return $ While cond stmts

forStatement :: Parser Stmt
forStatement = annotate $ do
  reserved "for"
  ini <- statement
  cond <- expression
  semi
  inc <- expression
  stmts <- bracedStatements
  return $ For ini cond inc stmts

ifElseStatement :: Parser Stmt
ifElseStatement = annotate $ do
  If cond stmts1 _ <- ifStatement
  reserved "else"
  stmts2 <- bracedStatements
  return $ IfElse cond stmts1 stmts2

ifStatement :: Parser Stmt
ifStatement = annotate $ do
  reserved "if"
  cond <- expression
  stmts <- bracedStatements
  return $ If cond stmts

initStatement :: Parser Stmt
initStatement = annotate $ do
  varType <- typeName
  Set var expr _ <- assignment
  semi
  return $ Init varType var expr

declStatement :: Parser Stmt
declStatement = annotate $ do
  varType <- typeName
  var <- identifier
  semi
  return $ Decl varType var

basicStatement :: Parser Stmt
basicStatement = annotate $ do
  expr <- expression
  semi
  return $ Expr expr

returnStatement :: Parser Stmt
returnStatement = annotate $ do
  reserved "return"
  expr <- optionMaybe expression
  semi
  return $ Return expr

bracedStatements :: Parser Stmt
bracedStatements = braces statements

statements :: Parser Stmt
statements = annotate . liftM Seq $ many statement

expression :: Parser Expr
expression = buildExpressionParser operators terminals
         <?> "expression"

operators :: OperatorTable Char st Expr
operators =
  [ prefixOp  "-"   (UnaryOp Neg)
  , prefixOp  "¬"   (UnaryOp Not)
  , prefixOp  "--"  (UnaryOp PreDec)
  , postfixOp "--"  (UnaryOp PostDec)
  , prefixOp  "++"  (UnaryOp PreInc)
  , postfixOp "++"  (UnaryOp PostInc)
  , infixOp   "^"   (BinaryOp Exp)     AssocLeft
  , infixOp   "*"   (BinaryOp Mul)     AssocLeft
  , infixOp   "/"   (BinaryOp Div)     AssocLeft
  , infixOp   "%"   (BinaryOp Mod)     AssocLeft
  , infixOp   "|"   (BinaryOp Divides) AssocLeft
  , infixOp   "+"   (BinaryOp Add)     AssocLeft
  , infixOp   "-"   (BinaryOp Sub)     AssocLeft
  , infixOp   "="   (BinaryOp Eq)      AssocNone
  , infixOp   "=/=" (BinaryOp Ineq)    AssocNone
  , infixOp   "<"   (BinaryOp Lt)      AssocNone
  , infixOp   ">"   (BinaryOp Gt)      AssocNone
  , infixOp   "<="  (BinaryOp LtEq)    AssocNone
  , infixOp   ">="  (BinaryOp GtEq)    AssocNone
  , infixOp   "and" (BinaryOp And)     AssocLeft
  , infixOp   "or"  (BinaryOp Or)      AssocLeft
  ]
  where
    -- TODO: Work out why this doesn't typecheck:
    -- opParser s expr = annotate $ reservedOp s >> return expr
    opParser  s expr       = do
      start <- getPosition
      reservedOp s
      end <- getPosition
      return . expr $ Source start end
    prefixOp  s expr       = [Prefix  (opParser s expr)]
    postfixOp s expr       = [Postfix (opParser s expr)]
    infixOp   s expr assoc = [Infix   (opParser s expr) assoc]

terminals :: Parser Expr
terminals = parens expression
        <|> lambda
        <|> try functionCall
        <|> try assignment
        <|> annotate (liftM BoolLit boolean)
        <|> annotate (liftM (IntLit . fromIntegral) integer)
        <|> annotate (liftM Var identifier)
        <?> "terminal expression"

lambda :: Parser Expr
lambda = annotate $ do
  params <- commaSep param
  reservedOp ":"
  retType <- typeName
  reservedOp "->"
  body <- funcBody
  let (types, names) = unzip params
  return $ Lambda (FuncType types retType) names body

param :: Parser (Type, Identifier)
param = do
  t <- typeName
  n <- identifier
  return (t, n)

funcBody :: Parser FuncBody
funcBody = (expression       >>= return . ExprBody)
       <|> (bracedStatements >>= return . StmtBody)
       <?> "function body"

assignment :: Parser Expr
assignment = annotate $ do
  var <- identifier
  reservedOp ":="
  expr <- expression
  return $ Set var expr

functionCall :: Parser Expr
functionCall = annotate $ do
  func <- identifier
  args <- parens $ commaSep expression
  return $ FuncCall func args

typeName :: Parser Type
typeName = (reserved "int"  >> return Int)
       <|> (reserved "bool" >> return Bool)
       <|> (reserved "void" >> return Void)
       <|> (reserved "func" >> return Func)
       <?> "type name"

boolean :: Parser Bool
boolean = (reserved "true"  >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean literal"
