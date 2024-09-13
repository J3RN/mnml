module Tokenizer
    ( tokenize
    , Token
    ) where

import           Data.Char        (GeneralCategory (LineSeparator),
                                   generalCategory, isSpace)
import           Data.Functor     (($>))
import           Data.Text

import           Text.Parsec      (Parsec, char, choice, digit, letter, many, many1, noneOf, sepBy, string, (<|>))
import           Text.Parsec.Char (satisfy)

type Parser = Parsec Text ()

data Token
  = TNewline
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TType
  | TTypeAlias
  | TEqual
  | TStringLiteral Text
  | TInteger Integer
  | TFloat Double
  | TRBrace
  | TLBrace
  | TComma
  | TIdentifier Text
  | TUnderscore
           -- deriving (Eq)

tokenize :: Parser [Token]
tokenize = sepBy token (many1 nbsp)

token :: Parser Token
token = choice [ tNewline
               , tLParen
               , tRParen
               , tLBracket
               , tRBracket
               , tLBrace
               , tRBrace
               , tComma
               , tType
               , tTypeAlias
               , tEqual
               , tStringLiteral
               , tFloat
               , tInteger
               , tIdentifier
               , tUnderscore
               ]

-- Non-breaking space

nbsp :: Parser Char
nbsp = satisfy (\c -> isSpace c && (generalCategory c /= LineSeparator))

-- Token parsers

tNewline :: Parser Token
tNewline = satisfy ((== LineSeparator) . generalCategory) $> TNewline

tLParen :: Parser Token
tLParen = char '(' $> TLParen

tRParen :: Parser Token
tRParen = char ')' $> TRParen

tLBracket :: Parser Token
tLBracket = char '[' $> TLBracket

tRBracket :: Parser Token
tRBracket = char ']' $> TRBracket

tLBrace :: Parser Token
tLBrace = char '{' $> TLBrace

tRBrace :: Parser Token
tRBrace = char '}' $> TRBrace

tComma :: Parser Token
tComma = char ',' $> TComma

tType :: Parser Token
tType = string "type" $> TType

tTypeAlias :: Parser Token
tTypeAlias = string "type alias" $> TTypeAlias

tEqual :: Parser Token
tEqual = char '=' $> TEqual

tUnderscore :: Parser Token
tUnderscore = char '_' $> TUnderscore

-- String literal
tStringLiteral :: Parser Token
tStringLiteral = do
    _ <- char '"'
    str <- many (noneOf "\"")
    _ <- char '"'
    return $ TStringLiteral (pack str)

-- Integer parser
tInteger :: Parser Token
tInteger = do
    num <- many1 digit
    return $ TInteger (read num)

-- Float parser
tFloat :: Parser Token
tFloat = do
    intPart <- many1 digit
    _ <- char '.'
    fracPart <- many1 digit
    return $ TFloat (read (intPart ++ "." ++ fracPart))

-- Identifier parser
tIdentifier :: Parser Token
tIdentifier = do
    first <- letter
    rest <- many (letter <|> digit <|> char '_')
    return $ TIdentifier (pack (first:rest))
