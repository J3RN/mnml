module MNML.Generate
    ( codeGen
    ) where

import           Control.Monad.State (State)
import           Data.Text           (Text)
import           MNML
import qualified MNML.Core           as Core
import qualified MNML.Parse          as Parse
import qualified MNML.Unify          as Unify

newtype GenerateError
  = UnificationError Unify.UnificationError

newtype GenerateState
  = GenerateState { modu :: Core.Module }

codeGen :: Text -> Text -> State CompilerState (Either [GenerateError] Text)
codeGen modName valName =
  either (map UnificationError) (const (generate modName valName)) <$> Unify.valueType modName valName

generate :: Text -> Text -> State CompilerState (Either [GenerateError] Text)
generate modName valName = do
  valDef <- lift (P.valueDef modName valName)

