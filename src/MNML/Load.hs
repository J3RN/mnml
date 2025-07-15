module MNML.Load
    ( load
    ) where

import           Data.Text      (Text)
-- import           MNML.Base            (QualifiedReference)
import qualified MNML.Constrain as Constrain
import           MNML.Error     (Fallible)
import qualified MNML.Parse     as Parse
import qualified MNML.Store     as Store
import qualified MNML.Unify     as Unify

load :: Text -> Fallible ()
load code = Parse.parse code >>= Constrain.constrain >>= Unify.unify >>= Store.store
