module MNML.Store
    ( store
    ) where

import           Control.Monad.Except (throwError)
import qualified MNML.AST.Type        as TAST
import           MNML.Error           (Error (StoreError), Fallible,
                                       StoreError (Placeholder''))

store :: TAST.Batch -> Fallible ()
store _batch = throwError [StoreError Placeholder'']
