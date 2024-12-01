module MNML
    ( CompilerState (..)
    , Modules
    , emptyState
    , moduleDefCache
    , nextTypeId
    , stateModules
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
import qualified MNML.AST.Span       as SAST
import           MNML.Base           (QualifiedValueReference)
import qualified MNML.Constraint     as C
import           MNML.Type           (Type)
import qualified MNML.Type           as Type

-- The compiler state contains caches and the type variable counter

type Cache = Map

type Modules = Cache Text Text

type ModuleDefCache = Cache Text [SAST.Definition]

type ValueDefCache = Cache QualifiedValueReference SAST.Expr

type ValueConstraintsCache = Cache QualifiedValueReference (Type, [C.Constraint])

data CompilerState
  = CompilerState
      { _modules               :: Modules
      , _nextTypeId            :: Type.VarId
      , _moduleDefCache        :: ModuleDefCache
      , _valueDefCache         :: ValueDefCache
      , _valueConstraintsCache :: ValueConstraintsCache
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _modules = Map.empty
    , _nextTypeId = 0
    , _moduleDefCache = Map.empty
    , _valueDefCache = Map.empty
    , _valueConstraintsCache = Map.empty
    }

-- Assorted lenses

stateModules :: Lens' CompilerState Modules
stateModules = lens _modules (\cs mods -> cs {_modules = mods})

nextTypeId :: Lens' CompilerState Type.VarId
nextTypeId = lens _nextTypeId (\cs vi -> cs {_nextTypeId = vi})

moduleDefCache :: Lens' CompilerState ModuleDefCache
moduleDefCache = lens _moduleDefCache (\cs mdc -> cs {_moduleDefCache = mdc})

valueDefCache :: Lens' CompilerState ValueDefCache
valueDefCache = lens _valueDefCache (\cs vdc -> cs {_valueDefCache = vdc})

valueConstraintsCache :: Lens' CompilerState ValueConstraintsCache
valueConstraintsCache = lens _valueConstraintsCache (\cs vcc -> cs {_valueConstraintsCache = vcc})

-- Not a great name, but
varIdPlusPlus :: StateT m (State CompilerState) Type.VarId
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
