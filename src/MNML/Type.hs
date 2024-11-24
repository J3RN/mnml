module MNML.Type
    ( FieldSpec
    , Trait (..)
    , Type (..)
    , VarId
    ) where

import           Data.Map  (Map)
import           Data.Set  (Set)
import           Data.Text (Text)

type VarId = Integer

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
