module MNML.Type
    ( ConcreteType (..)
    , TraitId
    , Type (..)
    , VarId
    ) where

import           Data.Text
import           MNML.AST  (NodeId)

type VarId = Integer
type TraitId = Integer


data Type
  -- "User"
  = NamedType Text
  -- Some type variable corresponding to an unaliased type
  | Var Text VarId
  | Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  deriving (Eq, Show)
