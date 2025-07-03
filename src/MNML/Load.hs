module MNML.Load
    ( load
    ) where
import           Control.Monad.State (State)
import           Data.Text           (Text)
import           MNML.Base           (QualifiedReference)
import           MNML.CompilerState  (CompilerState)
import           MNML.Constrain      (ConstraintError)
import qualified MNML.Constraint     as Constraint
import           MNML.Parse          (ParseError)
import qualified MNML.Parse          as Parse
import           MNML.Unify          (UnificationError)
import qualified MNML.Unify          as Unify

data Error
  = ParseError ParseError
  | UnificationError UnificationError

load :: Text -> State CompilerState (Either Error ())
load code = do
    parseRes <- Parse.parse code
    case parseRes of
        Left pError -> return (Left (ParseError pError))
        Right defs  -> do
            constraintRes <- Constrain.constain defs
            case constraintRes of
                Left cError          -> return (Left (ConstraintError cError))
                Right constraintSets -> _

parse :: Text -> State CompilerState
