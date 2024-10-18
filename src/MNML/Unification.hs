module MNML.Unification
    ( valueType
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, evalState, get, gets, put,
                                      runState)
import           Data.Either         (isLeft)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState, NodeId)
import qualified MNML.AST            as AST
import qualified MNML.Type           as T

data UnificationError
  = UnknownVar NodeId
  | UnknownVal Text
  | UnknownConstructor NodeId
  | TooFewArguments NodeId
  | TooManyArguments NodeId
  | NotFunctionType T.Type NodeId
  | ListInconsistentType NodeId
  | UnificationError NodeId NodeId

data UnificationState
  = UnificationState
      { _bindings :: Map.Map Text T.Type
      , _module   :: Text
      }

bindings :: Lens' UnificationState (Map.Map Text T.Type)
bindings = lens _bindings (\us bin -> us {_bindings = bin})

-- unify :: Text -> Text -> State CompilerState (Either UnificationError Type)
-- unify mod valName = do
--   cState <- get
--   return $ case valueDef cState mod valName of
--              Just expr -> evalState (exprType' expr) (UnificationState Map.empty mod)
--              Nothing -> Left (UnknownVal valName)

exprType' :: AST.Expr -> State UnificationState (Either UnificationError T.Type)

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
  let elemT = foldM (foldType nodeId) (Right T.Generic) elems
  in gets (evalState elemT)

-- This was meant to be a where, but I got a cryptic error
foldType :: NodeId -> Either UnificationError T.Type -> AST.Expr -> State UnificationState (Either UnificationError T.Type)
foldType nodeId t e =
          if isLeft t then return t
          else do
            uniS <- get
            let (elemT, newUniS) = runState (exprType' e) uniS
            put newUniS
            case (t, elemT) of
              (Left err, _)        -> return $ Left err
              (_, Left err)        -> return $ Left err
              (Right t1, Right t2) -> return $ maybe (Left $ ListInconsistentType nodeId) Right (unify t1 t2)

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
