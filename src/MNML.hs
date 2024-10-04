module MNML
    ( CompilerState (..)
    , Modules
    , NodeId
    , SourceSpan (..)
    , Type (..)
    , stateModules
    , stateSpans
    , stateTypes
    ) where

import           Data.Map    (Map)
import           Data.Text   (Text)
import           Lens.Micro  (Lens', lens)
import           Text.Parsec (SourcePos)

type NodeId = Integer

data SourceSpan
  = SourceSpan
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      }
  deriving (Eq, Show)

data Type
  = TInt
  | TFloat
  | TChar
  | TString
  | TNamedType Text -- "User"
  | TList Type -- TInt
  | TFun [Type] Type -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type)] -- [("name", TString), ...]
  | TGeneric -- "a"
  deriving (Eq, Show)

-- data Type
--   = TInt NodeId
--   | TFloat NodeId
--   | TChar NodeId
--   | TString NodeId
--   | TNamedType Text NodeId -- "User"
--   | TList Type NodeId -- TInt
--   | TFun [Type] Type NodeId -- [TInt, TInt] -> TInt
--   | TRecord [(Text, Type)] NodeId -- [("name", TString), ...]
--   | TGeneric NodeId -- "a"
--   deriving (Eq, Show)

-- The compiler state, contains various information about terms, etc
type Modules = Map Text Text

type Spans = Map NodeId SourceSpan

type Types = Map NodeId Type

data CompilerState
  = CompilerState
      { _stateSpans   :: Spans
      , _stateModules :: Modules
      , _stateTypes   :: Types
      }
  deriving (Eq, Show)

stateSpans :: Lens' CompilerState Spans
stateSpans = lens _stateSpans (\cs ss -> cs {_stateSpans = ss})

stateModules :: Lens' CompilerState Modules
stateModules = lens _stateModules (\cs sm -> cs {_stateModules = sm})

stateTypes :: Lens' CompilerState Types
stateTypes = lens _stateTypes (\cs sm -> cs {_stateTypes = sm})
