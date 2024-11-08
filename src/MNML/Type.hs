module MNML.Type
    ( Constraint (..)
    , Trait (..)
    , Type (..)
    , VarId
    ) where

import           Data.Text (Text)
import           MNML.AST  (NodeId)

type VarId = Integer

data Constraint
  = CEqual NodeId Type Type
  deriving (Show)

data Trait = Numeric
  deriving (Eq, Ord, Show)

data Type
  = Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  | AlgebraicType Text
  | TypeAlias Text Type
  -- Type var "a" requiring types to implement traits
  | Var Text [Trait] VarId
  deriving (Eq, Ord, Show)
