module MNML.Pattern
    ( Literal (..)
    , Pattern (..)
    ) where

import           Data.Text (Text)

data Pattern
  = Var Text -- "a"
  | Discard -- _
  | Constructor Text [Pattern]
  | Record [(Text, Pattern)]
  | List [Pattern]
  | Literal Literal
  deriving (Eq, Show)

data Literal
  = LInt Integer
  | LFloat Double
  | LChar Char
  | LString Text
  deriving (Eq, Show)
