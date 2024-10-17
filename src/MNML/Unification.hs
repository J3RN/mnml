module MNML.Unification
    ( valueType
    ) where

import           Control.Monad.State (State, evalState, get, put, runState)
import qualified Data.Map            as Map
import           Data.Maybe          (isNothing)
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState, NodeId)
import qualified MNML.AST            as AST
import           MNML.Type           as T

data UnificationError
  = UnknownVar NodeId
  | UnknownVal Text
  | UnknownConstructor NodeId
  | TooFewArguments NodeId
  | TooManyArguments NodeId
  | NotFunctionType Type NodeId
  | ListInconsistentType NodeId
  | UnificationError NodeId NodeId

data UnificationState
  = UnificationState
      { _bindings :: Map.Map Text Type
      , _module   :: Text
      }

bindings :: Lens' UnificationState (Map.Map Text Type)
bindings = lens _bindings (\us bin -> us {_bindings = bin})

-- unify :: Text -> Text -> State CompilerState (Either UnificationError Type)
-- unify mod valName = do
--   cState <- get
--   return $ case valueDef cState mod valName of
--              Just expr -> evalState (exprType' expr) (UnificationState Map.empty mod)
--              Nothing -> Left (UnknownVal valName)

exprType' :: AST.Expr -> State UnificationState (Either UnificationError Type)

exprType' (AST.EVar name nodeId) = do
  UnificationState {_bindings = b} <- get
  return $ maybe (Left (UnknownVar nodeId)) Right (b Map.!? name)

exprType' (AST.EConstructor name nodeId) = do
  UnificationState{_module = modu} <- get
  return $ maybe (Left $ UnknownConstructor nodeId) Right (constructorFunType modu name)

exprType' (AST.ELit lit _) = return $ Right $ litType lit

exprType' (AST.ELambda args body _) = do
  uniState <- get
  let argTypes = Prelude.map (, T.Generic) args
      scopedUniState = over bindings (Map.union (Map.fromList argTypes)) uniState
      (bodyType, finalState) = runState (exprType' body) scopedUniState
  -- put finalState
  let finalArgTypes = (_bindings finalState Map.!) <$> args
  return $ T.Fun finalArgTypes <$> bodyType

exprType' (AST.EApp fun args nodeId)          = do
  uniState <- get
  let funType = evalState (exprType' fun) uniState
  return $ case funType of
             Left err                       -> Left err
             Right (T.Fun argTypes resType) -> if length args > length argTypes then Left (TooManyArguments nodeId)
                                               else if length args < length argTypes then Left (TooFewArguments nodeId)
                                                    -- TODO: Unify args and argTypes
                                                    else Right resType
             Right otherType                -> Left (NotFunctionType otherType nodeId)

exprType' (AST.ECase _subj _branches _)    = _
exprType' (AST.EBinary _op _left _right _) = _
exprType' (AST.ERecord _fields _)          = _
exprType' (AST.EList elems nodeId)             =
      do
        uniState <- get
        let (elemsType, _) = foldl foldType (Just T.Generic, uniState) elems
        case elemsType of
          Nothing -> Left (ListInconsistentType nodeId)
          Just t  -> Right t
  where foldType (t, uniS) e =
          if isNothing t then (t, uniS)
          else let (elemT, newUniS) = runState (exprType' e) uniS
               in case (t, elemT) of
                    (Nothing, _)       -> (Nothing, newUniS)
                    (_, Nothing)       -> (Nothing, newUniS)
                    (Just t1, Just t2) -> (unify t1 t2, newUniS)

litType :: AST.Literal -> T.Type
litType (AST.LInt _ _)    = T.Int
litType (AST.LFloat _ _)  = T.Float
litType (AST.LChar _ _)   = T.Char
litType (AST.LString _ _) = T.String

unify :: T.Type -> T.Type -> Maybe T.Type
unify T.Generic t = Just t
unify t T.Generic = Just t
unify t1 t2       = Nothing

valueType :: CompilerState -> Text -> Text -> Either UnificationError Type
valueType cState modu valName =
  -- TODO: Check cache, have a cache, etc
  case valueDef cState modu valName of
    Just expr -> evalState (exprType' expr) (UnificationState Map.empty modu)
    Nothing   -> Left (UnknownVal valName)

valueDef :: CompilerState -> Text -> Text -> Maybe AST.Expr
valueDef cState modu valName = _

constructorFunType :: Text -> Text -> Maybe T.Type
constructorFunType modu conName = _
