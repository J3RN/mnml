module MNML.Constraint
    ( Constraint (..)
    ) where

import           MNML.AST.Span (SourceSpan)
import           MNML.Type     (Type)

data Constraint
  = CEqual SourceSpan Type Type
  deriving (Eq, Show)
