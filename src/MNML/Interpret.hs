module MNML.Interpret
    ( interpret
    ) where
import           Control.Monad.Except (throwError)
import           MNML.AST.Type        as TAST
import           MNML.Base            (QualifiedValueReference)
import           MNML.Error           (Error (RuntimeError), Fallible,
                                       RuntimeError (Placeholder))
import qualified MNML.Type            as T


interpret :: QualifiedValueReference -> Fallible (TAST.Expr T.Type)
interpret _qvr = throwError [RuntimeError Placeholder]
