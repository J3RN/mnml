module MNML.Type
    ( Constraint (..)
    , Type (..)
    , Trait (..)
    , VarId
    ) where

import           Data.Text
import           MNML.AST     (NodeId)
import           MNML.Pattern (Pattern)

type VarId = Integer

data Constraint
  -- CEqual Expected Actual
  = CEqual Type Type NodeId
  -- | CPattern Type Pattern NodeId
  deriving (Show)

data Trait = Numeric
  deriving (Show, Ord, Eq)

data Type
  -- "User" (can be an alias or a user-defined type)
  = TypeAlias Text
  | AlgebraicType Text
  -- Type var "a" requiring types to implement traits
  | Var Text [Trait] VarId
  | Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  deriving (Eq, Ord, Show)
