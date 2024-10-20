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
  | CPattern Type Pattern NodeId
  -- Weak sauce pre-trait constaint
  | CNumeric Type NodeId

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
