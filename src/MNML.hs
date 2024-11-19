module MNML
    ( CompilerState (..)
    , Modules
    , SourceSpan (..)
    , emptyState
    , moduleDefCache
    , nextNodeId
    , nextTypeId
    , stateModules
    , stateSpans
    , stateTypes
    , valueConstraintsCache
    , valueDefCache
    , varIdPlusPlus
    , writeThrough
    ) where

import           Control.Monad.State (State, StateT, gets, lift, modify, state)
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, (^.))
import           MNML.AST            (NodeId)
import qualified MNML.AST            as AST
import           MNML.Type           (Type)
import qualified MNML.Type           as T
import           Text.Parsec         (SourcePos)

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
      { _spans                 :: Spans
      , _modules               :: Modules
      , _types                 :: Types
      , _nextNodeId            :: AST.NodeId
      , _nextTypeId            :: T.VarId
      , _moduleDefCache        :: ModuleDefCache
      , _valueDefCache         :: ValueDefCache
      , _valueConstraintsCache :: ValueConstraintsCache
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _spans = Map.empty
    , _modules = Map.empty
    , _types = Map.empty
    , _nextNodeId = 0
    , _nextTypeId = 0
    , _moduleDefCache = Map.empty
    , _valueDefCache = Map.empty
    , _valueConstraintsCache = Map.empty
    }

-- Assorted lenses

stateSpans :: Lens' CompilerState Spans
stateSpans = lens _spans (\cs spans -> cs {_spans = spans})

stateModules :: Lens' CompilerState Modules
stateModules = lens _modules (\cs mods -> cs {_modules = mods})

stateTypes :: Lens' CompilerState Types
stateTypes = lens _types (\cs types -> cs {_types = types})

nextNodeId :: Lens' CompilerState AST.NodeId
nextNodeId = lens _nextNodeId (\cs ni -> cs {_nextNodeId = ni})

nextTypeId :: Lens' CompilerState T.VarId
nextTypeId = lens _nextTypeId (\cs vi -> cs {_nextTypeId = vi})

moduleDefCache :: Lens' CompilerState ModuleDefCache
moduleDefCache = lens _moduleDefCache (\cs mdc -> cs {_moduleDefCache = mdc})

valueDefCache :: Lens' CompilerState ValueDefCache
valueDefCache = lens _valueDefCache (\cs vdc -> cs {_valueDefCache = vdc})

valueConstraintsCache :: Lens' CompilerState ValueConstraintsCache
valueConstraintsCache = lens _valueConstraintsCache (\cs vcc -> cs {_valueConstraintsCache = vcc})

-- Not a great name, but
varIdPlusPlus :: StateT m (State CompilerState) T.VarId
varIdPlusPlus = lift (state (\s -> (_nextTypeId s, s {_nextTypeId = _nextTypeId s + 1})))

-- Cache ops

writeThrough ::
  (Ord key) =>
  Lens' CompilerState (Map key val) ->
  (key -> State CompilerState (Either err val)) ->
  key ->
  State CompilerState (Either err val)
writeThrough l calc key = do
  maybeVal <- gets ((!? key) . (^. l))
  case maybeVal of
    Just val -> return (Right val)
    Nothing -> do
      res <- calc key
      case res of
        Right val -> modify (over l (Map.insert key val)) >> return (Right val)
        Left err  -> return (Left err)
