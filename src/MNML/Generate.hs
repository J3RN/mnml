module MNML.Generate
    ( generate
    ) where

import           Control.Monad.State (State)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState, QualifiedValueReference)
import           MNML.AST.Type       (typeOf)
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
initialState =
  GenerateState {_modu = Core.Module {_memories = [], _functions = []}}

generate ::
  QualifiedValueReference ->
  State CompilerState (Either [GenerateError] Text)
generate qvr = do
  typeRes <- Unify.valueType qvr
  case typeRes of
    Left errs -> return (Left (UnificationError <$> errs))
    Right tvds ->
      return
        ( Right
            ((Text.pack . show . _modu) (foldl generateValue initialState tvds))
        )

generateValue :: GenerateState -> C.TypedValueDecl -> GenerateState
generateValue state tvd@(_, value) =
  let retType = typeToType (typeOf value)
      f =
        Core.Function
          { _params = []
          , _locals = []
          , _return = retType
          , _body = generateBody state tvd
          }
   in over (modu . Core.functions) (f :) state

generateBody :: GenerateState -> C.TypedValueDecl -> [Core.Expr]
-- A value being a var can only mean that it's a local reference
generateBody state ((m, _), TAST.EVar name _) =
  [ Core.Expr
      { _callee = "call"
      , _args = [Left (qvrToName (m, name))]
      }
  ]
generateBody state ((m, valName), TAST.EConstructor name (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.ELit lit _span) =
  case lit of
    TAST.LInt i _ -> [Core.Expr {_callee = "i32.const", _args = [Left (Text.pack (show i))]}]
    TAST.LFloat f _ -> [Core.Expr {_callee = "f64.const", _args = [Left (Text.pack (show f))]}]
-- TODO: For all others, promote to data section; use reference
-- Except type variables, dunno what to do about that.
-- TAST.TChar SourceSpanType
-- TAST.TString SourceSpanType
-- This one actually shouldn't exist
-- TAST.TNamedType Text SourceSpanType
-- TAST.TList Type SourceSpanType
-- TAST.TFun [Type] Type SourceSpanType
-- TAST.TRecord [(Text, Type)] SourceSpanType
-- Perhaps this one also
-- TAST.TVar Text SourceSpanType

generateBody state ((m, valName), TAST.ELambda params body (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.EApp fun args (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.ECase subj branches (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.EBinary op left right (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.ERecord fields (TAST.SourceSpanType {_type = t})) = _
generateBody state ((m, valName), TAST.EList elems (TAST.SourceSpanType {_type = t})) = _

qvrToName :: QualifiedValueReference -> Text
qvrToName (m, valName) = mconcat [m, ".", valName]

typeToType :: T.Type -> Core.Type
typeToType T.Int   = Core.I32
typeToType T.Float = Core.F64
-- typeToType T.Char  = Core.I32 -- FIXME: Use unicode characters
-- typeToType T.Fun argTs retT = Core.FR -- Uhh, we don't know the functions name?  How can we do a funcref?
typeToType _       = Core.DataPointer
