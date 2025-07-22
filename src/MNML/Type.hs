module MNML.Type
    ( FieldSpec
    , Trait (..)
    , Type (..)
    , VarId
    ) where

import           Data.Map  (Map)
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (Text)
import qualified Data.Text as Text

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
  | AlgebraicType Text VarId
  | TypeAlias Text Type
  -- Type var "a" requiring types to implement traits
  | Var Text (Set Trait) VarId
  -- A "partial record"; similar to a variable with traits
  | PartialRecord FieldSpec VarId
  deriving (Eq, Ord)

instance Show Type where
  show Int                      = "Int"
  show Float                    = "Float"
  show Char                     = "Char"
  show String                   = "String"
  show (List t)                 = "[" <> show t <> "]"
  show (Fun argTs retT)         = "(" <> concatMap show argTs <> ") -> " <> show retT
  show (Record fieldSpec)       = "Record(" <> show fieldSpec <> ")"
  show (AlgebraicType name vId) = Text.unpack name <> varId vId
  show (TypeAlias name _t)      = Text.unpack name
  show (Var name traits vId)    = Text.unpack name <> (if traits /= Set.empty then show (Set.toList traits) else "") <> varId vId
  show (PartialRecord fieldSpec vId) = "PartialRecord(" <> show fieldSpec <> ")" <> varId vId

varId :: VarId -> String
varId vid = map varId' (show vid)
  where varId' '1' = '¹'
        varId' '2' = '²'
        varId' '3' = '³'
        varId' '4' = '⁴'
        varId' '5' = '⁵'
        varId' '6' = '⁶'
        varId' '7' = '⁷'
        varId' '8' = '⁸'
        varId' '9' = '⁹'
        varId' '0' = '⁰'
        varId' _   = '*'
