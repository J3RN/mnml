module MNML.Store
    ( store
    ) where

import qualified MNML.AST.Type as TAST
import           MNML.Base     (QualifiedReference)
import           MNML.Error    (Fallible)

store :: QualifiedReference -> [TAST.Expr] -> Fallible ()
store = _
