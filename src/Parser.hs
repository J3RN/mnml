module Parser where

import           Data.Text
import           Text.Parsec

-- Data Types

type Parser = Parsec Text ()

data Declaration = TypeDecl Text [(Text, [Type])]
                 | TypeAliasDecl Text [(Text, Type)]
                 | ValueDecl Text Expr

data Type = TInt
          | TFloat
          | TChar
          | TString
          | TList Type
          | TFun [Type] Type          -- [TInt, TInt] -> TInt
          | TRecord [(Text, Type)]    -- [("name", TString), ...]
          | TNamedType Text           -- "User"
          deriving (Show)

data Expr = EVar Text
          | ELit Literal
          | ELambda [Text] Expr         -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
          | EApp Expr [Expr]              -- (EVar "fun") [(EVar "x"), (EVar "y")]
          | ECase Expr [(Pattern, Expr)]
          | EBinary Expr Operator Expr
          | ERecord [(Text, Expr)]
          | EList [Expr]
          deriving (Show)

data Literal = LInt Integer
             | LFloat Double
             | LChar Char
             | LString Text
             deriving (Show)

data Pattern = PVar Text
             | PConstructor Text [Pattern]
             | PRecord [(Text, Pattern)]
             | PList [Pattern]
             | PLiteral Literal
             deriving (Show)

data Operator = Add | Sub | Mul | Div | Pipe deriving (Show)

-- Top-level Parsers
mod :: Parser [Declaration]
mod = many decl

decl :: Parser Declaration
decl = typeDecl <|> typeAliasDecl <|> valueDecl

typeDecl :: Parser Declaration
typeDecl = do
  _ <- text "data"
  _ <- many1 whiteSpace
  name <- typeIdentifier
  _ <- many1 whiteSpace
  _ <- text "="
  _ <- many1 whiteSpace
  constructors <- sepBy1 (many1 whiteSpace *> text "," many1 whiteSpace) constructor
  return (TypeDecl name constructors)

constructor :: Parser (String, [Type])
constructor = _

typeAliasDecl :: Parser Declaration
typeAliasDecl = _

valueDecl :: Parser Declaration
valueDecl = _

-- Basic Parsers

identifier :: Parser Text
identifier = (:) <$> letter <*> many (alphaNum <|> char '_')

typeIdentifier :: Parser Text
typeIdentifier = (:) <$> upper <*> many alphaNum

-- Type Parsers

pType :: Parser Type
pType = try funType
    <|> try recordType
    <|> listType
    <|> simpleType

funType :: Parser Type
funType = do
    argTypes <- parens (pType `sepBy` (char ','))
    _ <- string "->"
    returnType <- pType
    TFun argTypes <$> returnType

recordType :: Parser Type
recordType = do
    fields <- braces (fieldDecl `sepBy` (char ','))
    return $ TRecord fields

fieldDecl :: Parser (Text, Type)
fieldDecl = do
    fieldName <- identifier
    _ <- char ':'
    fieldType <- pType
    return (fieldName, fieldType)

listType :: Parser Type
listType = do
    _ <- char '['
    t <- pType
    _ <- char ']'
    return $ TList t

simpleType :: Parser Type
simpleType = (TInt <$ string "Int")
         <|> (TFloat <$ string "Float")
         <|> (TChar <$ string "Char")
         <|> (TString <$ string "String")
         <|> (TData <$> typeIdentifier <*> pure [])

-- Expression Parsers

expression :: Parser Expr
expression = try caseExpr
         <|> lambdaExpr
         <|> appExpr
         <|> binaryExpr
         <|> simpleExpr

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- char '('
    params <- identifier `sepBy` (char ',')
    _ <- string "=>"
    body <- expression
    _ <- char ')'
    return $ ELambda params body

caseExpr :: Parser Expr
caseExpr = do
    _ <- string "case"
    expr <- expression
    _ <- string "of"
    cases <- braces (caseBranch `sepBy` (char ';'))
    return $ ECase expr cases

caseBranch :: Parser (Pattern, Expr)
caseBranch = do
    pat <- pattern
    _ <- string "->"
    expr <- expression
    return (pat, expr)

appExpr :: Parser Expr
appExpr = do
    func <- identifier
    args <- parens (expression `sepBy` (char ','))
    return $ EApp (EVar func) args

binaryExpr :: Parser Expr
binaryExpr = do
    left <- simpleExpr
    op <- operator
    right <- simpleExpr
    EBinary left op right

operator :: Parser Operator
operator = (Add <$ char '+')
       <|> (Sub <$ char '-')
       <|> (Mul <$ char '*')
       <|> (Div <$ char '/')

simpleExpr :: Parser Expr
simpleExpr = EVar <$> identifier
         <|> ELit <$> literal
         <|> parens expression

literal :: Parser Literal
literal = try (LFloat <$> float)
      <|> (LInt <$> integer)
      <|> (LChar <$> charLiteral)
      <|> (LString <$> stringLiteral)

-- Pattern Parsers

pattern :: Parser Pattern
pattern = try constructorPattern
      <|> recordPattern
      <|> listPattern
      <|> literalPattern
      <|> PVar <$> identifier

constructorPattern :: Parser Pattern
constructorPattern = do
    con <- typeIdentifier
    pats <- option [] (parens (pattern `sepBy` (char ',')))
    return $ PConstructor con pats

recordPattern :: Parser Pattern
recordPattern = do
    fields <- braces (fieldPattern `sepBy` (char ','))
    return $ PRecord fields

listPattern :: Parser Pattern
listPattern = do
    pats <- brackets (pattern `sepBy` (char ','))
    return $ PList pats

literalPattern :: Parser Pattern
literalPattern = PLiteral <$> literal

fieldPattern :: Parser (Text, Pattern)
fieldPattern = do
    name <- identifier
    _ <- char ':'
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
stringLiteral = between (char '"') (char '"') (many $ noneOf "\"")

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')
