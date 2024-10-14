module MNML.Type
    ( Type (..)
    ) where

import           Data.Text

type TVarId = Integer

data Type
  = Int
  | Float
  | Char
  | String
  | NamedType Text -- "User"
  | List Type -- TInt
  | Fun [Type] Type -- [TInt, TInt] -> TInt
  | Record [(Text, Type)] -- [("name", TString), ...]
  | Var Text TVarId -- "a"
  | Generic -- Maybe not needed (replace by above?)
  deriving (Eq, Show)
