module MNML.Type
    ( Type (..)
    ) where

import           Data.Text

type VarId = Integer
type TraitId = Integer

data Type
  = Int
  | Float
  | Char
  | String
  | NamedType Text -- "User"
  | List Type -- TInt
  | Fun [Type] Type -- [TInt, TInt] -> TInt
  | Record [(Text, Type)] -- [("name", TString), ...]
  | Var Text VarId -- "a"
  | Generic
  | Trait TraitId -- A type that implements a trait (concrete type unknown)
  deriving (Eq, Show)
