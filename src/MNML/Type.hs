module MNML.Type
    ( Constraint (..)
    , TraitId
    , Type (..)
    , VarId
    ) where

import           Data.Text
import           MNML.AST     (NodeId)
import           MNML.Pattern (Pattern)

type VarId = Integer

type TraitId = Integer

data Constraint
  -- CEqual Expected Actual
  = CEqual Type Type NodeId
  -- | CPattern Type Pattern NodeId
  deriving (Show)

data Type
  -- "User" (can be an alias or a user-defined type)
  = TypeAlias Text
  | AlgebraicType Text
  -- Generic type var "a"
  | Var Text VarId
  | Int
  | Float
  -- Weak sauce "I implement trait type"
  | Numeric
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  deriving (Eq, Ord, Show)
