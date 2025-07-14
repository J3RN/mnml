module MNML.Error
    ( ConstrainError (..)
    , Error (..)
    , Fallible
    , GenerateError (..)
    , ParseError (..)
    , RuntimeError (..)
    , StoreError (..)
    , UnificationError (..)
    ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (State)
import           Data.Set             (Set)
import           Data.Text            (Text)
import qualified MNML.AST.Span        as SAST
import           MNML.Base            (QualifiedConstructorReference,
                                       QualifiedTypeReference,
                                       QualifiedValueReference)
import           MNML.CompilerState   (CompilerState)
import qualified MNML.Type            as T
import qualified Text.Parsec          as Parsec

type Fallible a = ExceptT [Error] (State CompilerState) a

data Error
  = ParseError ParseError
  | ConstrainError ConstrainError
  | UnificationError UnificationError
  | GenerateError GenerateError
  | RuntimeError RuntimeError
  | StoreError StoreError
  deriving (Eq, Show)

data ParseError
  = ParsecError Parsec.ParseError
  | ModuleNotFound Text
  | ValueNotFound QualifiedValueReference
  deriving (Eq, Show)

data ConstrainError
  = UnknownConstructor QualifiedConstructorReference SAST.SourceSpan
  | UnknownType QualifiedTypeReference SAST.SourceSpan
  deriving (Eq, Show)

data UnificationError
  = ArgumentLengthMismatch SAST.SourceSpan
  | UnifyError T.Type T.Type SAST.SourceSpan
  | OccursError T.Type T.Type SAST.SourceSpan
  | ExpectedTraits T.Type (Set T.Trait) SAST.SourceSpan
  | ExpectedFields T.Type T.FieldSpec SAST.SourceSpan
  deriving (Eq, Show)

data GenerateError = Placeholder'
  deriving (Eq, Show)

data RuntimeError = Placeholder
  deriving (Eq, Show)

data StoreError = Placeholder''
  deriving (Eq, Show)
