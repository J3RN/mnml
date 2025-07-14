module MNML.Interpret
    ( interpret
    ) where
import           MNML.AST.Type as TAST
import           MNML.Base     (QualifiedValueReference)
import           MNML.Error    (Error (RuntimeError), Fallible)


interpret :: QualifiedValueReference -> Fallible TAST.Expr
interpret = _
