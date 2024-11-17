module MNML.Type
    ( Constraint (..)
    , FieldSpec
    , Trait (..)
    , Type (..)
    , VarId
    ) where

import           Data.Map  (Map)
import           Data.Set  (Set)
import           Data.Text (Text)
import           MNML.AST  (NodeId)

type VarId = Integer

data Constraint
  = CEqual NodeId Type Type
  deriving (Eq, Show)

-- Temporary until we get user-defined traits
data Trait = Numeric
  deriving (Eq, Ord, Show)

type FieldSpec = Map Text Type

data Type
  = Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record FieldSpec
  | AlgebraicType Text
  | TypeAlias Text Type
  -- Type var "a" requiring types to implement traits
  | Var Text (Set Trait) VarId
  -- A "partial record"; similar to a variable with traits
  | PartialRecord FieldSpec VarId
  deriving (Eq, Ord, Show)
