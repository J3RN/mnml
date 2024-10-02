module MNML
    ( CompilerState (..)
    ) where

import           Data.Map    (Map)
import           Data.Text   (Text)
import           MNML.Parser (SourceSpan)

-- The compiler state, contains various information about terms, etc
data CompilerState
  = CompilerState
      { _stateSpans   :: Map Text SourceSpan
      , _stateModules :: Map Text Text
      }
