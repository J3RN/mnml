module MNML.Constraint
    ( Constraint (..)
    ) where

import qualified MNML.Annotation as Anno
import qualified MNML.Type       as T

data Constraint
  = CEqual Anno.SpanAnnotation T.Type T.Type
  deriving (Eq, Show)
