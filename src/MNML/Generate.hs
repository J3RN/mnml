module MNML.Generate
    ( generate
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, execState, modify)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Lens.Micro          (Lens', lens, over)
import           MNML.AST.Type       (typeOf)
import qualified MNML.AST.Type       as TAST
import           MNML.Base           (QualifiedReference)
import           MNML.CompilerState  (CompilerState)
import qualified MNML.Core           as Core
import           MNML.Error          (Error (GenerateError), Fallible,
                                      GenerateError)
import qualified MNML.Type           as T
import qualified MNML.Unify          as Unify

newtype GenerateState
  = GenerateState { _modu :: Core.Module }

type Generate = State GenerateState

modu :: Lens' GenerateState Core.Module
modu = lens _modu (\gs m -> gs {_modu = m})

initialState :: GenerateState
initialState =
  GenerateState {_modu = Core.Module {_memories = [], _functions = []}}

generate :: QualifiedReference -> Fallible Text
generate qvr = do
  typeRes <- Unify.valueType qvr
  return $ case typeRes of
             Left errs -> Left (UnificationError <$> errs)
             Right tvds -> Right (Text.pack (show (_modu (execState (mapM generateValue tvds) initialState))))

-- TODO: Maybe return the function and have the caller place it in the module
generateValue :: TAST.TypedValueDef -> Generate ()
generateValue tvd@(_, value) = do
  body <- generateBody tvd
  let retType = typeToType (typeOf value)
      f =
        Core.Function
          { _params = []
          , _locals = []
          , _return = retType
          , _body = body
          }
  modify (over (modu . Core.functions) (f :))

generateBody :: TAST.TypedValueDef -> Generate [Core.Expr]
-- A value being a var can only mean that it's a local reference
generateBody ((m, _), TAST.EVar name _) = return [ Core.Expr { _callee = "call" , _args = [Left (qvrToName (m, name))]}]
generateBody ((m, valName), TAST.EConstructor name (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.ELit lit _span) =
  case lit of
    TAST.LInt i _ -> return [Core.Expr {_callee = "i32.const", _args = [Left (Text.pack (show i))]}]
    TAST.LFloat f _ -> return [Core.Expr {_callee = "f64.const", _args = [Left (Text.pack (show f))]}]
    -- TODO: For all others, promote to data section; use reference
    -- Except type variables, dunno what to do about that.
    -- TAST.TChar SourceSpanType
    -- TAST.TString SourceSpanType
    -- TAST.TList Type SourceSpanType
    -- TAST.TFun [Type] Type SourceSpanType
    -- TAST.TRecord [(Text, Type)] SourceSpanType
    -- Perhaps this one also
    -- TAST.TVar Text SourceSpanType

generateBody ((m, valName), TAST.ELambda params body (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.EApp fun args (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.ECase subj branches (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.EBinary op left right (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.ERecord fields (TAST.SourceSpanType {_type = t})) = _
generateBody ((m, valName), TAST.EList elems (TAST.SourceSpanType {_type = t})) = _

qvrToName :: QualifiedReference -> Text
qvrToName (m, valName) = mconcat [m, ".", valName]

typeToType :: T.Type -> Core.Type
typeToType T.Int   = Core.I32
typeToType T.Float = Core.F64
-- typeToType T.Char  = Core.I32 -- FIXME: Use unicode characters
-- typeToType T.Fun argTs retT = Core.FR -- Uhh, we don't know the functions name?  How can we do a funcref?
typeToType _       = Core.DataPointer
