module MNML.CompilerState
    ( CompilerState (..)
    , definitions
    , emptyState
    , nextTypeId
    , typeDefinitions
    , varIdPlusPlus
    ) where

import           Control.Monad.State (State, state)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Lens.Micro          (Lens', lens)

import qualified MNML.AST.Type       as TAST
import           MNML.Base           (QualifiedReference)
import qualified MNML.Type           as Type

-- The compiler state contains caches and the type variable counter

data CompilerState
  = CompilerState
      { _definitions     :: Map QualifiedReference TAST.Expr
      , _typeDefinitions :: Map QualifiedReference Type.Type
      , _nextTypeId      :: Type.VarId
      }
  deriving (Eq, Show)

emptyState :: CompilerState
emptyState =
  CompilerState
    { _definitions = Map.empty
    , _typeDefinitions = Map.empty
    , _nextTypeId = 0
    }

-- Assorted lenses

definitions :: Lens' CompilerState (Map QualifiedReference TAST.Expr)
definitions = lens _definitions (\cs mods -> cs {_definitions = mods})

typeDefinitions :: Lens' CompilerState (Map QualifiedReference Type.Type)
typeDefinitions = lens _typeDefinitions (\cs mods -> cs {_typeDefinitions = mods})

nextTypeId :: Lens' CompilerState Type.VarId
nextTypeId = lens _nextTypeId (\cs vi -> cs {_nextTypeId = vi})

-- Not a great name, but
varIdPlusPlus :: State CompilerState Type.VarId
varIdPlusPlus = state (\s -> (_nextTypeId s, s {_nextTypeId = _nextTypeId s + 1}))
