module Parser
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Parser.parse
    , Type (..)
    ) where

import           Data.Char   (GeneralCategory (LineSeparator), generalCategory,
                              isSpace)
import           Data.Text
import           Text.Parsec

-- Data Types

type Parser = Parsec Text ()

data Declaration
  = TypeDecl Text [(Text, [Type])]
  | TypeAliasDecl Text [(Text, Type)]
  | ValueDecl Text Expr
  deriving (Show)

data Type
  = TInt
  | TFloat
  | TChar
  | TString
  | TNamedType Text -- "User"
  | TList Type -- TInt
  | TFun [Type] Type -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type)] -- [("name", TString), ...]
  | TGeneric -- "a"
  deriving (Show)

data Expr
  = EVar Text
  | ELit Literal
  | ELambda [Text] Expr -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)]
  | EBinary Expr Operator Expr
  | ERecord [(Text, Expr)]
  | EList [Expr]
  deriving (Show)

data Literal
  = LInt Integer
  | LFloat Double
  | LChar Char
  | LString Text
  deriving (Show)

data Pattern
  = PVar Text
  | PConstructor Text [Pattern]
  | PRecord [(Text, Pattern)]
  | PList [Pattern]
  | PLiteral Literal
  deriving (Show)

data Operator = Add | Sub | Mul | Div | Pipe
  deriving (Show)

-- Client API
parse :: Text -> Text -> Either ParseError [Declaration]
parse fName = runParser Parser.mod () (unpack fName)

-- Top-level Parsers
mod :: Parser [Declaration]
mod = do
  _ <- many whiteSpace
  manyTill (decl <* many whiteSpace) eof

decl :: Parser Declaration
decl = typeDecl <|> typeAliasDecl <|> valueDecl

typeDecl :: Parser Declaration
typeDecl = do
  name <- typeIdentifier
  _ <- many whiteSpace
  _ <- string "="
  _ <- many whiteSpace
  constructors <- sepBy1 constructor (many whiteSpace *> char '|' *> many whiteSpace)
  return (TypeDecl name constructors)

constructor :: Parser (Text, [Type])
constructor = do
  name <- typeIdentifier
  _ <- many whiteSpace
  cData <- parens (commaSep pType) <|> pure []
  return (name, cData)

typeAliasDecl :: Parser Declaration
typeAliasDecl = do
  _ <- string "alias"
  _ <- many1 whiteSpace
  name <- typeIdentifier
  _ <- many whiteSpace
  _ <- char '='
  _ <- many whiteSpace
  fields <- braces (commaSep fieldDecl)
  return (TypeAliasDecl name fields)

valueDecl :: Parser Declaration
valueDecl = do
  name <- identifier
  _ <- many1 whiteSpace
  _ <- char '='
  _ <- many1 whiteSpace
  ValueDecl name <$> expression

-- Basic Parsers

identifier :: Parser Text
identifier = pack <$> ((:) <$> letter <*> many (alphaNum <|> char '_'))

typeIdentifier :: Parser Text
typeIdentifier = pack <$> ((:) <$> upper <*> many alphaNum)

-- Type Parsers

pType :: Parser Type
pType =
  try funType
    <|> try recordType
    <|> listType
    <|> simpleType

funType :: Parser Type
funType = do
  argTypes <- parens (commaSep pType)
  _ <- many whiteSpace
  _ <- string "->"
  _ <- many whiteSpace
  returnType <- pType
  return (TFun argTypes returnType)

recordType :: Parser Type
recordType = do
  fields <- braces (commaSep fieldDecl)
  return $ TRecord fields

fieldDecl :: Parser (Text, Type)
fieldDecl = do
  fieldName <- identifier
  _ <- many whiteSpace >> char ':' >> many whiteSpace
  fieldType <- pType
  return (fieldName, fieldType)

listType :: Parser Type
listType = TList <$> brackets pType

simpleType :: Parser Type
simpleType =
  (TInt <$ string "Int")
    <|> (TFloat <$ string "Float")
    <|> (TChar <$ string "Char")
    <|> (TString <$ string "String")
    <|> (TNamedType <$> typeIdentifier)

-- Expression Parsers

expression :: Parser Expr
expression =
  try caseExpr
    <|> lambdaExpr
    <|> appExpr
    -- <|> binaryExpr
    <|> simpleExpr

lambdaExpr :: Parser Expr
lambdaExpr = do
  _ <- char '('
  params <- commaSep identifier
  _ <- char ')'
  _ <- many whiteSpace
  _ <- string "=>"
  _ <- many whiteSpace
  _ <- char '{'
  _ <- many whiteSpace
  body <- expression
  _ <- many whiteSpace
  _ <- char '}'
  return $ ELambda params body

caseExpr :: Parser Expr
caseExpr = do
  _ <- string "case"
  _ <- many1 whiteSpace
  expr <- expression
  _ <- many1 whiteSpace
  _ <- string "of" >> newline
  cases <- braces (caseBranch `sepBy` newline)
  return $ ECase expr cases

caseBranch :: Parser (Pattern, Expr)
caseBranch = do
  pat <- pattern
  _ <- many whiteSpace
  _ <- string "->"
  _ <- many whiteSpace
  expr <- expression
  return (pat, expr)

appExpr :: Parser Expr
appExpr = do
  func <- identifier
  args <- parens (commaSep expression)
  return $ EApp (EVar func) args

-- TODO: Totally overhaul
-- binaryExpr :: Parser Expr
-- binaryExpr = do
--     left <- simpleExpr
--     op <- operator
--     right <- simpleExpr
--     return (EBinary left op right)

operator :: Parser Operator
operator =
  (Add <$ char '+')
    <|> (Sub <$ char '-')
    <|> (Mul <$ char '*')
    <|> (Div <$ char '/')

simpleExpr :: Parser Expr
simpleExpr =
  EVar <$> identifier
    <|> ELit <$> literal
    <|> parens expression

literal :: Parser Literal
literal =
  try (LFloat <$> float)
    <|> (LInt <$> integer)
    <|> (LChar <$> charLiteral)
    <|> (LString <$> stringLiteral)

-- Pattern Parsers

pattern :: Parser Pattern
pattern =
  try constructorPattern
    <|> recordPattern
    <|> listPattern
    <|> literalPattern
    <|> PVar <$> identifier

constructorPattern :: Parser Pattern
constructorPattern = do
  con <- typeIdentifier
  pats <- option [] (parens (commaSep pattern))
  return $ PConstructor con pats

recordPattern :: Parser Pattern
recordPattern = do
  fields <- braces (commaSep fieldPattern)
  return $ PRecord fields

-- TODO: revisit
listPattern :: Parser Pattern
listPattern = do
  pats <- brackets (commaSep pattern)
  return $ PList pats

literalPattern :: Parser Pattern
literalPattern = PLiteral <$> literal

fieldPattern :: Parser (Text, Pattern)
fieldPattern = do
  name <- identifier
  _ <- many1 whiteSpace
  _ <- char ':'
  _ <- many1 whiteSpace
  pat <- pattern
  return (name, pat)

-- Helper Parsers

integer :: Parser Integer
integer = read <$> many1 digit

float :: Parser Double
float = do
  intPart <- many1 digit
  _ <- char '.'
  fracPart <- many1 digit
  return $ read (intPart ++ "." ++ fracPart)

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') anyChar

stringLiteral :: Parser Text
stringLiteral = between (char '"') (char '"') (pack <$> many (noneOf "\""))

parens :: Parser a -> Parser a
parens = between (char '(' >> many whiteSpace) (many whiteSpace >> char ')')

braces :: Parser a -> Parser a
braces = between (char '{' >> many whiteSpace) (many whiteSpace >> char '}')

brackets :: Parser a -> Parser a
brackets = between (char '[' >> many whiteSpace) (many whiteSpace >> char ']')

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (try (many whiteSpace *> char ',' *> many whiteSpace))

nbsp :: Parser Char
nbsp = satisfy (\c -> isSpace c && (generalCategory c /= LineSeparator))

-- newline :: Parser Char
-- newline = satisfy (\c ->generalCategory c == LineSeparator)

whiteSpace :: Parser Char
whiteSpace = satisfy isSpace
