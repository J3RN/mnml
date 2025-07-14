module MNML.Store
    ( store
    ) where

import qualified MNML.AST.Type as TAST
import           MNML.Error    (Fallible)

store :: TAST.Batch -> Fallible ()
store = _
