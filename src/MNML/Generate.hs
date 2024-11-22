module MNML.Generate
    ( generate
    ) where

import           Control.Monad.State (State)
import           Data.List           (intercalate)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           MNML                (CompilerState, QualifiedValueReference)
import qualified MNML.Constrain      as C
import qualified MNML.Core           as Core
import qualified MNML.Unify          as Unify

newtype GenerateError
  = UnificationError Unify.UnificationError

newtype GenerateState
  = GenerateState { modu :: Core.Module }

generate :: QualifiedValueReference -> State CompilerState (Either [GenerateError] Text)
generate qvr = do
  typeRes <- Unify.valueType qvr
  case typeRes of
    Left errs  -> return (Left (UnificationError <$> errs))
    Right tvds -> return (Right (Text.pack (intercalate "\n" (map generate' tvds))))

generate' :: C.TypedValueDecl -> String
generate' = _
