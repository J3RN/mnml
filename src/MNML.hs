module MNML
    ( CompilerState (..)
    , Modules
    , SourceSpan (..)
    , emptyState
    , stateModules
    , stateSpans
    , stateTypes
    ) where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Text   (Text)
import           Lens.Micro  (Lens', lens)
import           MNML.AST    (NodeId)
import           MNML.Type   (Type)
import           Text.Parsec (SourcePos)

data SourceSpan
  = SourceSpan
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      }
  deriving (Eq, Show)

-- The compiler state, contains various information about terms, etc

type Modules = Map Text Text

type Spans = Map NodeId SourceSpan

type Types = Map NodeId Type

data CompilerState
  = CompilerState
      { _spans   :: Spans
      , _modules :: Modules
      , _types   :: Types
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _spans = Map.empty,
      _modules = Map.empty,
      _types = Map.empty
    }

-- Assorted lenses

stateSpans :: Lens' CompilerState Spans
stateSpans = lens _spans (\cs spans -> cs {_spans = spans})

stateModules :: Lens' CompilerState Modules
stateModules = lens _modules (\cs mods -> cs {_modules = mods})

stateTypes :: Lens' CompilerState Types
stateTypes = lens _types (\cs types -> cs {_types = types})
