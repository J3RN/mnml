module MNML.Error
    ( ConstrainError (..)
    , Error (..)
    , ParseError (..)
    , RuntimeError (..)
    , UnificationError (..)
    ) where

import           Data.Set      (Set)
import           Data.Text     (Text)
import qualified MNML.AST.Span as SAST
import           MNML.Base     (QualifiedReference)
import qualified MNML.Type     as T
import qualified Text.Parsec   as Parsec

data Error
  = ParseError ParseError
  | ConstrainError ConstrainError
  | UnificationError UnificationError
  | RuntimeError RuntimeError

data ParseError
  = ParsecError Parsec.ParseError
  | ModuleNotFound Text
  | ValueNotFound QualifiedReference
  deriving (Eq, Show)

data ConstrainError
  = UnknownConstructor Text -- SAST.SourceSpan
  | UnknownType Text -- SAST.SourceSpan
  deriving (Eq, Show)

data UnificationError
  = ArgumentLengthMismatch SAST.SourceSpan
  | UError T.Type T.Type SAST.SourceSpan
  | OccursError T.Type T.Type SAST.SourceSpan
  | ExpectedTraits T.Type (Set T.Trait) SAST.SourceSpan
  | ExpectedFields T.Type T.FieldSpec SAST.SourceSpan
  deriving (Eq, Show)

data RuntimeError = Placeholder
