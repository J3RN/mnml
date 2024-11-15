module MNML.Parse
    ( ParseError
    , parse
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, gets, lift, modify)
import           Data.Functor        (($>))
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState (..), SourceSpan (..),
                                      nextNodeId, stateSpans)
import           MNML.AST            (Declaration (..), Expr (..), Literal (..),
                                      NodeId, Operator (..), Pattern (..),
                                      Type (..))
import           Text.Parsec         (ParseError, ParsecT, SourcePos, alphaNum,
                                      char, eof, getPosition, getState, lower,
                                      many, many1, manyTill, modifyState, oneOf,
                                      option, runParserT, sepBy1, space, try,
                                      upper, (<|>))
import qualified Text.Parsec.Token   as Tok

-- Data Types

newtype ParseEnv
  = ParseEnv { _nextId :: NodeId }

nextId :: Lens' ParseEnv NodeId
nextId = lens _nextId (\pe ni -> pe {_nextId = ni})

type Parser = ParsecT Text ParseEnv (State CompilerState)

-- Client API
parse :: Text -> State CompilerState (Either ParseError [Declaration])
parse modu = do
  moduleTextResult <- gets ((Map.!? modu) . _modules)
  initialNodeId <- gets _nextNodeId
  let initialEnv = (ParseEnv {_nextId = initialNodeId})
  case moduleTextResult of
    Just rawCode ->
      runParserT
        (MNML.Parse.mod <* updateGlobalNextId)
        initialEnv
        (Text.unpack (modu <> ".mnml"))
        rawCode
    Nothing ->
      runParserT
        (fail (concat ["File '", Text.unpack modu <> ".mnml'", " not loaded"]))
        initialEnv
        (Text.unpack (modu <> ".mnml"))
        Text.empty
  where
    updateGlobalNextId :: Parser ()
    updateGlobalNextId = do
      ParseEnv {_nextId = nodeId} <- getState
      lift (modify (over nextNodeId (const nodeId)))

-- Helpers

-- Fix name later
nodeIdPlusPlus :: Parser NodeId
nodeIdPlusPlus = do
  ParseEnv {_nextId = nodeId} <- getState
  modifyState (over nextId (+ 1))
  return nodeId

recordSpan :: SourcePos -> SourcePos -> NodeId -> Parser ()
recordSpan start end nodeId = do
  let nodeSpan = SourceSpan start end
  lift (modify (over stateSpans (Map.insert nodeId nodeSpan)))

-- This function needs to record the span of p, retrieve the next NodeID, increment the node
-- counter, and associate the NodeID with the recorded span.
captureSpan :: Parser (NodeId -> b) -> Parser b
captureSpan p = do
  nodeId <- nodeIdPlusPlus
  start <- getPosition
  node <- p
  end <- getPosition
  recordSpan start end nodeId
  return (node nodeId)

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
        nodeId <- nodeIdPlusPlus
        recordSpan start end nodeId
        return (EApp expr params nodeId)
    )
    func
    apps

binaryExpr :: Parser Expr
binaryExpr =
  captureChainl1 boolExpr (EBinary <$> captureSpan (Equals <$ dEqual))

boolExpr :: Parser Expr
boolExpr =
  captureChainl1 termExpr (EBinary <$> captureSpan ((And <$ reserved "and") <|> (Or <$ reserved "or")))

termExpr :: Parser Expr
termExpr =
  captureChainl1 factorExpr (EBinary <$> captureSpan ((Add <$ plus) <|> (Sub <$ minus)))

factorExpr :: Parser Expr
factorExpr =
  captureChainl1 unaryExpr (EBinary <$> captureSpan ((Mul <$ star) <|> (Div <$ slash)))

captureChainl1 :: Parser a -> Parser (a -> a -> NodeId -> a) -> Parser a
captureChainl1 p op =
  do
    start <- getPosition
    x <- p
    rest start x
  where
    rest start x =
      do
        nodeId <- nodeIdPlusPlus
        f <- op
        y <- p
        end <- getPosition
        recordSpan start end nodeId
        let res = f x y nodeId in rest start res
        <|> return x

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
