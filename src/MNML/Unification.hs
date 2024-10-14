module MNML.Unification
    ( valueType
    ) where

import           Control.Monad.State (State, evalState, get, runState)
import           Data.Map            as Map
import           Data.Maybe          (maybe)
import           Data.Text           (Text)
import           MNML                (CompilerState, NodeId, stateModules)
import qualified MNML.AST            as AST
import           MNML.Type           as T

data UnificationError
  = UnknownVar NodeId
  | UnknownVal Text
  | UnknownConstructor NodeId

data UnificationState
  = UnificationState
      { _bindings :: Map Text Type
      , _module   :: Text
      }

-- unify :: Text -> Text -> State CompilerState (Either UnificationError Type)
-- unify mod valName = do
--   cState <- get
--   return $ case valueDef cState mod valName of
--              Just expr -> evalState (unify' expr) (UnificationState Map.empty mod)
--              Nothing -> Left (UnknownVal valName)

unify' :: AST.Expr -> State UnificationState (Either UnificationError Type)

unify' (AST.EVar name nodeId) = do
  UnificationState {_bindings = b} <- get
  return $ maybe (Left (UnknownVar nodeId)) Right (b Map.!? name)

unify' (AST.EConstructor name nodeId) = do
  UnificationState{_module = mod} <- get
  return $ maybe (Left $ UnknownConstructor nodeId) Right (constructorFunType mod name)

unify' (AST.ELit lit _) = pure $ Right $ unifyLit lit

unify' (AST.ELambda args body _) = do
  uniState <- get
  let argTypes = Prelude.map (, T.Generic) args
      (bodyType, finalState) = runState (unify' body) uniState
      -- TODO: Extract refined arg types
  return $ T.Fun [] <$> bodyType

unify' (AST.EApp _fun _args _)          = _
unify' (AST.ECase _subj _branches _)    = _
unify' (AST.EBinary _op _left _right _) = _
unify' (AST.ERecord _fields _)          = _
unify' (AST.EList _elems _)             = _

unifyLit :: AST.Literal -> T.Type
unifyLit (AST.LInt _ _)    = T.Int
unifyLit (AST.LFloat _ _)  = T.Float
unifyLit (AST.LChar _ _)   = T.Char
unifyLit (AST.LString _ _)=T.String

valueType :: CompilerState -> Text -> Text -> Either UnificationError Type
valueType cState mod valName =
  -- TODO: Check cache, have a cache, etc
  case valueDef cState mod valName of
    Just expr -> evalState (unify' expr) (UnificationState Map.empty mod)
    Nothing   -> Left (UnknownVal valName)

valueDef :: CompilerState -> Text -> Text -> Maybe AST.Expr
valueDef cState mod valName = _

constructorFunType :: Text -> Text -> Maybe T.Type
constructorFunType mod conName = _
