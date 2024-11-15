-- An IR that resembles WASM
module MNML.Core
    ( Expr (..)
    , Function (..)
    , Memory (..)
    , Modu (..)
    , Type (..)
    ) where

import           Data.Text (Text)

newtype Funcref
  = Funcref Text
data Expr
  = Expr
      { _callee :: Text
      , _args   :: [Either Text Expr]
      }

-- Ignoring vector types for the time being
data Type
  = I32
  | F64
  | FR Funcref

data Function
  = Function
      { _params :: [Type]
      , _locals :: [Type]
      , _return :: Type
      , _body   :: [Expr]
      }

newtype Memory
  = Memory (Maybe Int)

data Modu
  = Modu
      { memories  :: [Memory]
      , functions :: [Function]
      }

instance Show Expr where
  show e = concat $ "(" : show (_callee e) : map showArg (_args e) ++ [")"]
    where showArg (Left t)   = show t
          showArg (Right e') = show e'

instance Show Type where
  show I32                 = "i32"
  show F64                 = "f64"
  show (FR (Funcref name)) = concat ["(func ", show name, ")"]

instance Show Function where
  show f = concat $ "(func " : map show (_params f) ++ map show (_locals f) ++ [show (_return f)] ++ map show (_body f) ++ [")"]

instance Show Memory where
  show (Memory Nothing)     = "(memory)"
  show (Memory (Just size)) = concat ["(memory", show size, ")"]

instance Show Modu where
  show (Modu {memories = mems, functions = funcs }) = concat $ "(module " : map show mems ++ map show funcs ++ [")"]
