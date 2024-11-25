module MNML.Parse
    ( MNML.Parse.ParseError
    , moduleDef
    , valueDef
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, gets)
import           Data.Functor        (($>))
import qualified Data.Map            as Map
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           MNML                (CompilerState (..),
                                      QualifiedValueReference, moduleDefCache,
                                      valueDefCache, writeThrough)
import           MNML.AST.Span       (Declaration (..), Expr (..), Literal (..),
                                      Operator (..), Pattern (..),
                                      SourceSpan (..), Type (..))
import           Text.Parsec         (ParseError, ParsecT, alphaNum, char, eof,
                                      getPosition, lower, many, many1, manyTill,
                                      oneOf, option, runParserT, sepBy1, space,
                                      try, upper, (<|>))
import qualified Text.Parsec.Token   as Tok

-- Data Types

-- This used to be useful and now is not
type ParseEnv = ()

type Parser = ParsecT Text ParseEnv (State CompilerState)

data ParseError
  = ParseError Text.Parsec.ParseError
  | ModuleNotFound Text
  | ValueNotFound QualifiedValueReference
  deriving (Eq, Show)

-- Client API

valueDef :: QualifiedValueReference -> State CompilerState (Either MNML.Parse.ParseError Expr)
valueDef qvr = writeThrough valueDefCache findValDef qvr
  where
    findValDef (m, v) = do
      mDef <- moduleDef m
      return (mDef >>= findDef v)
    findDef name defs =
      maybe
        (Left (ValueNotFound qvr))
        Right
        ( findMaybe
            ( \case
                (ValueDecl n expr _) | n == name -> Just expr
                _ -> Nothing
            )
            defs
        )

moduleDef :: Text -> State CompilerState (Either MNML.Parse.ParseError [Declaration])
moduleDef = writeThrough moduleDefCache parse

-- Function to run the parser

parse :: Text -> State CompilerState (Either MNML.Parse.ParseError [Declaration])
parse modu = do
  moduleTextResult <- gets ((Map.!? modu) . _modules)
  case moduleTextResult of
    Just rawCode ->
      either (Left . MNML.Parse.ParseError) Right
        <$> runParserT
          MNML.Parse.mod
          ()
          (Text.unpack (modu <> ".mnml"))
          rawCode
    Nothing ->
      return (Left (ModuleNotFound modu))

-- Helpers

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

-- Helpers

-- This function needs to record the span of p
captureSpan :: Parser (SourceSpan -> b) -> Parser b
captureSpan p = do
  start <- getPosition
  node <- p
  end <- getPosition
  return (node (SourceSpan start end))

-- Top-level Parsers
mod :: Parser [Declaration]
mod = do
  _ <- many whiteSpace
  manyTill decl eof

decl :: Parser Declaration
decl = typeDecl <|> typeAliasDecl <|> valueDecl

typeDecl :: Parser Declaration
typeDecl = captureSpan $ do
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
typeAliasDecl = captureSpan $ do
  _ <- reserved "alias"
  expansionType <- pType
  _ <- reserved "as"
  name <- typeIdentifier
  return (TypeAliasDecl name expansionType)

valueDecl :: Parser Declaration
valueDecl = captureSpan $ do
  name <- identifier
  _ <- equal
  expr <- expression
  return (ValueDecl name expr)

-- Type Parsers

pType :: Parser Type
pType =
  funType
    <|> recordType
    <|> listType
    <|> simpleType
    <|> captureSpan (TNamedType <$> typeIdentifier)

funType :: Parser Type
funType = captureSpan $ do
  argTypes <- parens (commaSep pType)
  _ <- rArrow
  returnType <- pType
  return (TFun argTypes returnType)

recordType :: Parser Type
recordType = captureSpan $ do
  fields <- braces (commaSep fieldDecl)
  return (TRecord fields)

fieldDecl :: Parser (Text, Type)
fieldDecl = do
  fieldName <- identifier
  _ <- colon
  fieldType <- pType
  return (fieldName, fieldType)

listType :: Parser Type
listType = captureSpan (TList <$> brackets pType)

simpleType :: Parser Type
simpleType =
  captureSpan (TInt <$ reserved "Int")
    <|> captureSpan (TFloat <$ reserved "Float")
    <|> captureSpan (TChar <$ reserved "Char")
    <|> captureSpan (TString <$ reserved "String")

-- Expression Parsers

expression :: Parser Expr
expression = try binaryExpr <|> unaryExpr

unaryExpr :: Parser Expr
unaryExpr = try appExpr <|> primaryExpr

-- Any expression that is not an application
primaryExpr :: Parser Expr
primaryExpr =
  caseExpr
    -- lambda and generalized parens both start with open paren
    <|> try lambdaExpr
    <|> listExpr
    <|> recordExpr
    <|> captureSpan (EVar <$> identifier)
    <|> captureSpan (EConstructor <$> typeIdentifier)
    <|> captureSpan (ELit <$> literal)
    <|> parens expression

lambdaExpr :: Parser Expr
lambdaExpr = captureSpan $ do
  params <- parens (commaSep identifier)
  _ <- fatArrow
  body <- braces expression
  return (ELambda params body)

caseExpr :: Parser Expr
caseExpr = captureSpan $ do
  _ <- reserved "case"
  expr <- expression
  _ <- reserved "of"
  cases <- many1 caseBranch
  return (ECase expr cases)

caseBranch :: Parser (Pattern, Expr)
caseBranch = do
  pat <- pattern
  _ <- rArrow
  expr <- expression
  return (pat, expr)

appExpr :: Parser Expr
appExpr = do
  start <- getPosition
  func <- primaryExpr
  apps <-
    many1
      ( do
          params <- parens (commaSep expression)
          end <- getPosition
          return (params, end)
      )
  foldM
    ( \expr (params, end) -> do
        return (EApp expr params (SourceSpan start end))
    )
    func
    apps

binaryExpr :: Parser Expr
binaryExpr =
  captureChainl1 boolExpr (EBinary <$> (Equals <$ dEqual))

boolExpr :: Parser Expr
boolExpr =
  captureChainl1
    termExpr
    (EBinary <$> ((And <$ reserved "and") <|> (Or <$ reserved "or")))

termExpr :: Parser Expr
termExpr =
  captureChainl1 factorExpr (EBinary <$> ((Add <$ plus) <|> (Sub <$ minus)))

factorExpr :: Parser Expr
factorExpr =
  captureChainl1 unaryExpr (EBinary <$> ((Mul <$ star) <|> (Div <$ slash)))

captureChainl1 :: Parser a -> Parser (a -> a -> SourceSpan -> a) -> Parser a
captureChainl1 p op =
  do
    start <- getPosition
    lhs <- p
    rest start lhs
  where
    rest start lhs =
      do
        f <- op
        rhs <- p
        end <- getPosition
        let res = f lhs rhs (SourceSpan start end) in rest start res
        <|> return lhs

listExpr :: Parser Expr
listExpr = do
  captureSpan (EList <$> brackets (commaSep expression))

recordExpr :: Parser Expr
recordExpr = do
  captureSpan $ ERecord <$> braces (commaSep recordFieldExpr)

recordFieldExpr :: Parser (Text, Expr)
recordFieldExpr = do
  name <- identifier
  _ <- colon
  value <- expression
  return (name, value)

literal :: Parser Literal
literal =
  captureSpan $
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
    <|> captureSpan (symbol "_" $> PDiscard)
    <|> captureSpan (PVar <$> identifier)

constructorPattern :: Parser Pattern
constructorPattern = captureSpan $ do
  con <- typeIdentifier
  pats <- option [] (parens (commaSep pattern))
  return (PConstructor con pats)

recordPattern :: Parser Pattern
recordPattern = captureSpan $ do
  fields <- braces (commaSep fieldPattern)
  return (PRecord fields)

-- TODO: We need a cons operator or similar
listPattern :: Parser Pattern
listPattern = captureSpan $ do
  pats <- brackets (commaSep pattern)
  return (PList pats)

literalPattern :: Parser Pattern
literalPattern = captureSpan (PLiteral <$> literal)

fieldPattern :: Parser (Text, Pattern)
fieldPattern = do
  name <- identifier
  _ <- colon
  pat <- pattern
  return (name, pat)

-- "Lexer"

mnmlDef :: Tok.GenLanguageDef Text ParseEnv (State CompilerState)
mnmlDef =
  Tok.LanguageDef
    { Tok.caseSensitive = True
    , Tok.opStart = Tok.opLetter mnmlDef
    , Tok.opLetter = oneOf "+-*/|>="
    , Tok.commentStart = ""
    , Tok.commentEnd = ""
    , Tok.commentLine = ""
    , Tok.nestedComments = True
    , Tok.identStart = lower
    , Tok.identLetter = alphaNum <|> char '_'
    , Tok.reservedOpNames = ["+", "-", "*", "/", "|>", "=", "==", "|"]
    , Tok.reservedNames =
        [ "alias"
        , "as"
        , "case"
        , "of"
        , "not"
        , "and"
        , "or"
        , "Int"
        , "Float"
        , "Char"
        , "String"
        ]
    }

lexer :: Tok.GenTokenParser Text ParseEnv (State CompilerState)
lexer = Tok.makeTokenParser mnmlDef

identifier :: Parser Text
identifier = Text.pack <$> Tok.identifier lexer

-- Slight misnomer; also applies to constructors
typeIdentifier :: Parser Text
typeIdentifier = Tok.lexeme lexer (Text.cons <$> upper <*> (Text.pack <$> many (alphaNum <|> char '_')))

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

stringLiteral :: Parser Text
stringLiteral = Text.pack <$> Tok.stringLiteral lexer

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
