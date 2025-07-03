module MNML.Load
    ( load
    ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (State)
import           Data.Text            (Text)
import           MNML.Base            (QualifiedReference)
import           MNML.CompilerState   (CompilerState)
import           MNML.Constrain       (ConstraintError)
import qualified MNML.Constraint      as Constraint
import           MNML.Error           (Error)
import           MNML.Parse           (ParseError)
import qualified MNML.Parse           as Parse
import qualified MNML.Store           as Store
import qualified MNML.Unify           as Unify

load :: Text -> ExceptT Error (State CompilerState) ()
load code = do
  sast <- Parse.parse code
  constraintRes <- Constrain.constain defs
  unifyRes <- Unify.unify constraint
  Store.store unifyRes
