module MNML.Interpret
    ( interpret
    ) where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (State)
import           MNML.AST.Type        as TAST
import           MNML.CompilerState   (CompilerState)
import           MNML.Error           (Error (RuntimeError))


interpret :: ExceptT Error (State CompilerState) TAST.Expr
interpret = _
