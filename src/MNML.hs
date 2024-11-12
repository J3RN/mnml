module MNML
    ( CompilerState (..)
    , Modules
    , SourceSpan (..)
    , emptyState
    , stateModules
    , stateSpans
    , stateTypes
    , moduleDefCache
    , valueDefCache
    , valueConstraintsCache
    , writeThrough
    ) where

import Control.Monad.State
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Text   (Text)
import           Lens.Micro  (Lens', lens, (^.), over)
import           MNML.AST    (NodeId)
import qualified MNML.AST as AST
import           MNML.Type   (Type)
import qualified MNML.Type as T
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

type ModuleDefCache = Map Text [AST.Declaration]
type ValueDefCache = Map (Text, Text) AST.Expr
type ValueConstraintsCache = Map (Text, Text) (T.Type, [T.Constraint])

data CompilerState
  = CompilerState
      { _spans   :: Spans
      , _modules :: Modules
      , _types   :: Types
      , _moduleDefCache :: ModuleDefCache
      , _valueDefCache :: ValueDefCache
      , _valueConstraintsCache :: ValueConstraintsCache
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _spans = Map.empty,
      _modules = Map.empty,
      _types = Map.empty,
      _moduleDefCache = Map.empty,
      _valueDefCache = Map.empty,
      _valueConstraintsCache = Map.empty
    }

-- Assorted lenses

stateSpans :: Lens' CompilerState Spans
stateSpans = lens _spans (\cs spans -> cs {_spans = spans})

stateModules :: Lens' CompilerState Modules
stateModules = lens _modules (\cs mods -> cs {_modules = mods})

stateTypes :: Lens' CompilerState Types
stateTypes = lens _types (\cs types -> cs {_types = types})

moduleDefCache :: Lens' CompilerState ModuleDefCache
moduleDefCache = lens _moduleDefCache (\cs mdc -> cs {_moduleDefCache = mdc})

valueDefCache :: Lens' CompilerState ValueDefCache
valueDefCache = lens _valueDefCache (\cs vdc -> cs {_valueDefCache = vdc})

valueConstraintsCache :: Lens' CompilerState ValueConstraintsCache
valueConstraintsCache = lens _valueConstraintsCache (\cs vcc -> cs {_valueConstraintsCache = vcc})

-- Cache ops

writeThrough :: (CompilerState -> Maybe b) -> (b -> CompilerState -> CompilerState) -> (a -> b) -> a -> State CompilerState b
writeThrough getter setter calc key = do
  maybeVal <- gets getter
  case maybeVal of
    Just val -> return val
    Nothing -> let val = calc key
               in modify (setter val) >> return val

writeThrough' :: Lens' CompilerState val -> (key -> val) -> key -> State CompilerState val
writeThrough' lens calc key = do
  maybeVal <- gets (^. lens)
  case maybeVal of
    Just val -> return val
    Nothing -> let val = calc key
               in over lens (Map.insert key val)
