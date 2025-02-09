-- An IR that resembles WASM
module MNML.Core
    ( Expr (..)
    , Function (..)
    , Memory (..)
    , Module (..)
    , Type (..)
    , functions
    , memories
    ) where

import           Data.List  (intercalate)
import           Data.Text  (Text, unpack)
import           Lens.Micro (Lens', lens)

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
  | DataPointer
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

data Module
  = Module
      { _memories  :: [Memory]
      , _functions :: [Function]
      }

memories :: Lens' Module [Memory]
memories = lens _memories (\modu mems -> modu {_memories = mems})

functions :: Lens' Module [Function]
functions = lens _functions (\modu funs -> modu {_functions = funs})

instance Show Expr where
  show e = concat ["(", unpack (_callee e), " ", unwords (map showArg (_args e)), ")"]
    where
      showArg (Left t)   = unpack t
      showArg (Right e') = show e'

instance Show Type where
  show I32                 = "i32"
  show F64                 = "f64"
  show DataPointer         = "i64"
  show (FR (Funcref name)) = concat ["(func ", show name, ")"]

instance Show Function where
  show f =
    concat
      [ "(func "
      , unwords (map (parameterize . show) (_params f))
      , " "
      , unwords (map (localize . show) (_locals f))
      , " "
      , (returnize . show) (_return f)
      , "\n"
      , unlines (map show (_body f))
      , ")"
      ]
    where
      parameterize x = "(param " <> x <> ")"
      localize x = "(local " <> x <> ")"
      returnize x = "(return " <> x <> ")"

instance Show Memory where
  show (Memory Nothing)     = "(memory)"
  show (Memory (Just size)) = concat ["(memory", show size, ")"]

instance Show Module where
  show (Module {_memories = mems, _functions = funcs}) =
    concat
      [ "(module\n"
      , "  "
      , intercalate "\n" (map show mems)
      , "  "
      , intercalate "\n " (map show funcs)
      , ")"
      ]
