module MNML.Interpret
    ( interpret
    ) where
import           MNML.AST.Type as TAST
import           MNML.Base     (QualifiedReference)
import           MNML.Error    (Error (RuntimeError), Fallible)


interpret :: QualifiedReference -> Fallible TAST.Expr
interpret = _
