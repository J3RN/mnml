module MNML.Parser
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , parse
    ) where

import           Control.Monad.State (State, lift, modify)
import           Data.Functor        (($>))
import qualified Data.Map            as Map
import           Data.Text
import           Lens.Micro          (over)
import           MNML                (CompilerState (..), NodeId,
                                      SourceSpan (..), Type (..), stateSpans)
import           Text.Parsec         (ParseError, ParsecT, SourcePos, alphaNum,
                                      char, eof, getPosition, getState, lower,
                                      many, many1, manyTill, oneOf, option,
                                      putState, runParserT, sepBy1, space, try,
                                      upper, (<|>))
import qualified Text.Parsec.Token   as Tok

-- Data Types

newtype ParserEnv
  = ParserEnv { nextId :: NodeId }

type Parser = ParsecT Text ParserEnv (State CompilerState)

data Declaration
  = TypeDecl Text [(Text, [Type])] NodeId
  | TypeAliasDecl Text [(Text, Type)] NodeId
  | ValueDecl Text Expr NodeId
  deriving (Eq, Show)

data Expr
  = EVar Text NodeId
  | EConstructor Text NodeId
  | ELit Literal NodeId
  | ELambda [Text] Expr NodeId -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] NodeId -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)] NodeId
  | EBinary Operator Expr Expr NodeId
  | ERecord [(Text, Expr)] NodeId
  | EList [Expr] NodeId
  deriving (Eq, Show)

data Literal
  = LInt Integer NodeId
  | LFloat Double NodeId
  | LChar Char NodeId
  | LString Text NodeId
  deriving (Eq, Show)

data Pattern
  = PVar Text NodeId
  | PDiscard NodeId -- _
  | PConstructor Text [Pattern] NodeId
  | PRecord [(Text, Pattern)] NodeId
  | PList [Pattern] NodeId
  | PLiteral Literal NodeId
  deriving (Eq, Show)

data Operator
  = Add NodeId
  | Sub NodeId
  | Mul NodeId
  | Div NodeId
  | And NodeId
  | Or NodeId
  | Equals NodeId
  deriving (Eq, Show)

-- Client API
parse :: Text -> Text -> State CompilerState (Either ParseError [Declaration])
parse filename = runParserT MNML.Parser.mod (ParserEnv {nextId = 0}) (unpack filename)

-- Helpers

-- Fix name later
nodeIdPlusPlus :: Parser NodeId
nodeIdPlusPlus = do
  pe <- getState
  let nodeId = nextId pe
  putState (pe {nextId = nodeId + 1})
  return nodeId

recordSpan :: SourcePos -> SourcePos -> NodeId -> Parser ()
recordSpan start end nodeId = do
  let nodeSpan = SourceSpan start end
  lift $ modify (over stateSpans (Map.insert nodeId nodeSpan))

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
  name <- typeIdentifier
  _ <- equal
  fields <- braces (commaSep fieldDecl)
  return $ TypeAliasDecl name fields

valueDecl :: Parser Declaration
valueDecl = captureSpan $ do
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
expression = try binaryExpr <|> unaryExpr

unaryExpr :: Parser Expr
-- unaryExpr = try appExpr <|> primaryExpr
unaryExpr = primaryExpr

-- Any expression that is not an application
primaryExpr :: Parser Expr
primaryExpr =
  caseExpr
    -- lambda and generalized parens both start with open paren
    <|> try lambdaExpr
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
  return $ ELambda params body

caseExpr :: Parser Expr
caseExpr = captureSpan $ do
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

-- appExpr :: Parser Expr
-- appExpr = do
--   func <- primaryExpr
--   apps <- many1 (parens (commaSep expression))
--   return $ Prelude.foldl EApp func apps

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
    rest start x = do{ nodeId <- nodeIdPlusPlus;
                       f <- op;
                       y <- p;
                       end <- getPosition;
                       recordSpan start end nodeId;
                       let res = f x y nodeId in rest start res
                     }
                    <|> return x

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
  return $ PConstructor con pats

recordPattern :: Parser Pattern
recordPattern = captureSpan $ do
  fields <- braces (commaSep fieldPattern)
  return $ PRecord fields

-- TODO: We need a cons operator or similar
listPattern :: Parser Pattern
listPattern = captureSpan $ do
  pats <- brackets (commaSep pattern)
  return $ PList pats

literalPattern :: Parser Pattern
literalPattern = captureSpan $ PLiteral <$> literal

fieldPattern :: Parser (Text, Pattern)
fieldPattern = do
  name <- identifier
  _ <- colon
  pat <- pattern
  return (name, pat)

-- "Lexer"

mnmlDef :: Tok.GenLanguageDef Text (ParserEnv) (State CompilerState)
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

lexer :: Tok.GenTokenParser Text (ParserEnv) (State CompilerState)
lexer = Tok.makeTokenParser mnmlDef

identifier :: Parser Text
identifier = pack <$> Tok.identifier lexer

-- Slight misnomer; also applies to constructors
typeIdentifier :: Parser Text
typeIdentifier = Tok.lexeme lexer (Data.Text.cons <$> upper <*> (Data.Text.pack <$> many (alphaNum <|> char '_')))

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
