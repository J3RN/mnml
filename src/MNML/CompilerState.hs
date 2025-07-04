module MNML.CompilerState
    ( CompilerState (..)
    , emptyState
    , lookupType
    , lookupVal
    , nextTypeId
    , varIdPlusPlus
    ) where

import           Control.Monad.State (State, state)
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Lens.Micro          (Lens', lens)
import qualified MNML.AST.Type       as TAST
import           MNML.Base           (QualifiedReference)
import qualified MNML.Type           as T

-- The compiler state contains caches and the type variable counter

type Identifier = String

data CompilerState
  = CompilerState
      { _valueNames       :: Map QualifiedReference Identifier
      , _valueDefinitions :: Map Identifier TAST.Expr
      , _typeNames        :: Map QualifiedReference Identifier
      , _typeDefinitions  :: Map Identifier T.Type
      , _nextTypeId       :: T.VarId
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _valueNames = Map.empty
    , _valueDefinitions = Map.empty
    , _typeNames = Map.empty
    , _typeDefinitions = Map.empty
    , _nextTypeId = 0
    }

-- Assorted lenses

nextTypeId :: Lens' CompilerState T.VarId
nextTypeId = lens _nextTypeId (\cs vi -> cs {_nextTypeId = vi})

-- Helpers

-- Lookup a type by name
lookupType :: CompilerState -> QualifiedReference -> Maybe T.Type
lookupType cs qvr = do
  tId <- _typeNames cs !? qvr
  _typeDefinitions cs !? tId

-- Lookup a value by name
lookupVal :: CompilerState -> QualifiedReference -> Maybe TAST.Expr
lookupVal cs qvr = do
  vId <- _valueNames cs !? qvr
  _valueDefinitions cs !? vId

-- Not a great name, but
varIdPlusPlus :: State CompilerState T.VarId
varIdPlusPlus = state (\s -> (_nextTypeId s, s {_nextTypeId = _nextTypeId s + 1}))
