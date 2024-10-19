module MNML.Type
    ( Constraint (..)
    , TraitId
    , Type (..)
    , VarId
    ) where

import           Data.Text
import           MNML.AST  (NodeId)

type VarId = Integer
type TraitId = Integer

data Constraint
  = CType Type NodeId
  -- | CEqual VarId VarId

data Type
  -- "User" (can be an alias or a user-defined type)
  = NamedType Text
  -- Generic type var "a"
  | Var Text VarId
  | Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  deriving (Eq, Show)
