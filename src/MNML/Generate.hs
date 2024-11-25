module MNML.Generate
    ( generate
    ) where

import           Control.Monad.State (State)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState, QualifiedValueReference)
import qualified MNML.AST.Type       as TAST
import qualified MNML.Constrain      as C
import qualified MNML.Core           as Core
import qualified MNML.Type           as T
import qualified MNML.Unify          as Unify

newtype GenerateError
  = UnificationError Unify.UnificationError
  deriving (Eq, Show)

newtype GenerateState
  = GenerateState { _modu :: Core.Module }

modu :: Lens' GenerateState Core.Module
modu = lens _modu (\gs m -> gs {_modu = m})

initialState :: GenerateState
initialState = GenerateState {_modu = Core.Module {_memories = [], _functions = []}}

generate :: QualifiedValueReference -> State CompilerState (Either [GenerateError] Text)
generate qvr = do
  typeRes <- Unify.valueType qvr
  case typeRes of
    Left errs -> return (Left (UnificationError <$> errs))
    Right tvds -> return (Right ((Text.pack . show . _modu) (foldl generateValue initialState tvds)))

generateValue :: GenerateState -> C.TypedValueDecl -> GenerateState
-- A value being a var can only mean that it's a local reference
generateValue state ((m, valName), TAST.EVar name (TAST.SourceSpanType {_type = T.Int})) =
  let f =
        Core.Function
          { _params = []
          , _locals = []
          , _return = Core.I32
          , _body = [Core.Expr {_callee = "call", _args = [Left (qvrToName (m, name))]}]
          }
   in over (modu . Core.functions) (f :) state

qvrToName :: QualifiedValueReference -> Text
qvrToName (m, valName) = mconcat [m, ".", valName]
