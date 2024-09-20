module Parser
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Parser.parse
    , Pattern (..)
    , Type (..)
    ) where

import           Data.Functor          (($>))
import           Data.Functor.Identity (Identity)
import           Data.Text
import           Text.Parsec
import qualified Text.Parsec.Token     as Tok

-- Data Types

type Parser = Parsec Text ()

data Declaration
  = TypeDecl Text [(Text, [Type])]
  | TypeAliasDecl Text [(Text, Type)]
  | ValueDecl Text Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Expr
  = EVar Text
  | ELit Literal
  | ELambda [Text] Expr -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)]
  | EBinary Operator Expr Expr
  | EConstructor Text [Expr]
  | ERecord [(Text, Expr)]
  | EList [Expr]
  deriving (Eq, Show)

data Literal
  = LInt Integer
  | LFloat Double
  | LChar Char
  | LString Text
  deriving (Eq, Show)

data Pattern
  = PVar Text
  | PDiscard -- _
  | PConstructor Text [Pattern]
  | PRecord [(Text, Pattern)]
  | PList [Pattern]
  | PLiteral Literal
  deriving (Eq, Show)

data Operator = Add | Sub | Mul | Div | And | Or | Equals
  deriving (Eq, Show)

-- Client API
parse :: Text -> Text -> Either ParseError [Declaration]
parse fName = runParser Parser.mod () (unpack fName)

-- Top-level Parsers
mod :: Parser [Declaration]
mod = do
  _ <- many whiteSpace
  manyTill decl eof

decl :: Parser Declaration
decl = typeDecl <|> typeAliasDecl <|> valueDecl

typeDecl :: Parser Declaration
typeDecl = do
  name <- typeIdentifier
  _ <- equal
  constructors <- sepBy1 constructor bar
  return (TypeDecl name constructors)

constructor :: Parser (Text, [Type])
constructor = do
  name <- typeIdentifier
  cData <- parens (commaSep pType) <|> pure []
  return (name, cData)

typeAliasDecl :: Parser Declaration
typeAliasDecl = do
  _ <- reserved "alias"
  name <- typeIdentifier
  _ <- equal
  fields <- braces (commaSep fieldDecl)
  return $ TypeAliasDecl name fields

valueDecl :: Parser Declaration
valueDecl = do
  name <- identifier
  _ <- equal
  expr <- expression
  return $ ValueDecl name expr

-- Type Parsers

pType :: Parser Type
pType =
  funType
    <|> recordType
    <|> listType
    <|> simpleType
    <|> (TNamedType <$> typeIdentifier)

funType :: Parser Type
funType = do
  argTypes <- parens (commaSep pType)
  _ <- rArrow
  returnType <- pType
  return $ TFun argTypes returnType

recordType :: Parser Type
recordType = do
  fields <- braces (commaSep fieldDecl)
  return $ TRecord fields

fieldDecl :: Parser (Text, Type)
fieldDecl = do
  fieldName <- identifier
  _ <- colon
  fieldType <- pType
  return (fieldName, fieldType)

listType :: Parser Type
listType = TList <$> brackets pType

simpleType :: Parser Type
simpleType =
  (TInt <$ symbol "Int")
    <|> (TFloat <$ symbol "Float")
    <|> (TChar <$ symbol "Char")
    <|> (TString <$ symbol "String")

-- Expression Parsers

expression :: Parser Expr
expression =
  caseExpr
    <|> try lambdaExpr
    <|> constructorExpr
    <|> try appExpr
    <|> binaryExpr
    <|> recordExpr
    <|> simpleExpr

lambdaExpr :: Parser Expr
lambdaExpr = do
  params <- parens (commaSep identifier)
  _ <- fatArrow
  body <- braces expression
  return $ ELambda params body

constructorExpr :: Parser Expr
constructorExpr = do
  name <- typeIdentifier
  args <- option [] $ parens (commaSep expression)
  return $ EConstructor name args

caseExpr :: Parser Expr
caseExpr = do
  _ <- reserved "case"
  expr <- expression
  _ <- reserved "of"
  cases <- many1 caseBranch
  return $ ECase expr cases

caseBranch :: Parser (Pattern, Expr)
caseBranch = do
  pat <- pattern
  _ <- rArrow
  expr <- expression
  return (pat, expr)

appExpr :: Parser Expr
appExpr = do
  func <- identifier
  args <- parens (commaSep expression)
  return $ EApp (EVar func) args

binaryExpr :: Parser Expr
binaryExpr =
  chainl1 boolExpr (EBinary <$> (Equals <$ dEqual))

boolExpr :: Parser Expr
boolExpr =
  chainl1 termExpr (EBinary <$> ((And <$ reserved "and") <|> (Or <$ reserved "or")))

termExpr :: Parser Expr
termExpr =
  chainl1 factorExpr (EBinary <$> ((Add <$ plus) <|> (Sub <$ minus)))

factorExpr :: Parser Expr
factorExpr =
  chainl1 simpleExpr (EBinary <$> ((Mul <$ star) <|> (Div <$ slash)))

recordExpr :: Parser Expr
recordExpr = do
  ERecord <$> braces (commaSep recordFieldExpr)

recordFieldExpr :: Parser (Text, Expr)
recordFieldExpr = do
  name <- identifier
  _ <- colon
  value <- expression
  return (name, value)

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
    <|> symbol "_" $> PDiscard
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

-- TODO: We need a cons operator or similar
listPattern :: Parser Pattern
listPattern = do
  pats <- brackets (commaSep pattern)
  return $ PList pats

literalPattern :: Parser Pattern
literalPattern = PLiteral <$> literal

fieldPattern :: Parser (Text, Pattern)
fieldPattern = do
  name <- identifier
  _ <- colon
  pat <- pattern
  return (name, pat)

-- "Lexer"

mnmlDef :: Tok.GenLanguageDef Text () Identity
mnmlDef =
  Tok.LanguageDef
    { Tok.caseSensitive = True,
      Tok.opStart = Tok.opLetter mnmlDef,
      Tok.opLetter = oneOf "+-*/|>=",
      Tok.commentStart = "",
      Tok.commentEnd = "",
      Tok.commentLine = "",
      Tok.nestedComments = True,
      Tok.identStart = lower,
      Tok.identLetter = alphaNum <|> char '_',
      Tok.reservedOpNames = ["+", "-", "*", "/", "|>", "=", "==", "|"],
      Tok.reservedNames = ["alias", "case", "of", "not", "and", "or"]
    }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser mnmlDef

identifier :: Parser Text
identifier = pack <$> Tok.identifier lexer

-- Slight misnomer; also applies to constructors
typeIdentifier :: Parser Text
typeIdentifier = (Data.Text.cons <$> upper) <*> identifier

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

stringLiteral :: Parser Text
stringLiteral = pack <$> Tok.stringLiteral lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

fatArrow :: Parser String
fatArrow = symbol "=>"

rArrow :: Parser String
rArrow = symbol "->"

colon :: Parser String
colon = Tok.colon lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

equal :: Parser ()
equal = reservedOp "="

dEqual :: Parser ()
dEqual = reservedOp "=="

plus :: Parser ()
plus = reservedOp "+"

minus :: Parser ()
minus = reservedOp "-"

star :: Parser ()
star = reservedOp "*"

slash :: Parser ()
slash = reservedOp "/"

bar :: Parser ()
bar = reservedOp "|"

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser String
whiteSpace = many1 space
