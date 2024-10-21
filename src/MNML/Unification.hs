module MNML.Unification
    ( UnificationError
    , valueType
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, get, gets, lift, modify,
                                      put, runState, state)
import           Data.Bifunctor      (first)
import           Data.Either         (isLeft)
import           Data.Map            (Map, (!), (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, listToMaybe, mapMaybe)
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..))
import           MNML.AST            (NodeId)
import qualified MNML.AST            as AST
import qualified MNML.Parser         as P
import qualified MNML.Type           as T

data UnificationError
  = UnknownVar NodeId
  | UnknownVal Text
  | UnknownConstructor Text
  | TooFewArguments NodeId
  | TooManyArguments NodeId
  | NotFunctionType T.Type NodeId
  | ListInconsistentType NodeId
  | UnificationError T.Type T.Type NodeId
  | ParseError P.ParseError
  deriving (Eq, Show)

type Bindings = Map Text T.Type

data UnificationState
  = UnificationState
      { _bindings  :: Bindings
      , _module    :: Text
      , _nextVarId :: T.VarId
      , _errors    :: [UnificationError]
      }

initialEnv :: Text -> UnificationState
initialEnv modu =
  UnificationState
    { _bindings = Map.empty,
      _module = modu,
      _nextVarId = 0,
      _errors = []
    }

type Unify a = StateT UnificationState (State CompilerState) a

bindings :: Lens' UnificationState Bindings
bindings = lens _bindings (\us bin -> us {_bindings = bin})

addError :: UnificationError -> Unify ()
addError err = modify (\s -> s {_errors = err : _errors s})

giveUp :: UnificationError -> Text -> Unify (T.Type, [T.Constraint])
giveUp err name = addError err >> (,[]) <$> freshTypeVar name

constrain :: AST.Expr -> Unify (T.Type, [T.Constraint])
constrain (AST.EVar name nodeId) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- The presence of a var doesn't inform it's type
    Just t -> return (t, [])
    Nothing -> do
      addError (UnknownVar nodeId)
      newTVar <- freshTypeVar name
      return (newTVar, [])
constrain (AST.EConstructor name _nodeId) = do
  modu <- gets _module
  expectedTypeRes <- constructorFunType modu name
  case expectedTypeRes of
    Left err           -> giveUp err "name"
    Right expectedType -> return (expectedType, [])
constrain (AST.ELit lit _nodeId) =
  return (litType lit, [])
constrain (AST.ELambda args body nodeId) = do
  oldBindings <- gets _bindings
  -- Fresh argument type bindings
  argVars <- mapM (\arg -> (arg,) <$> freshTypeVar arg) args
  modify (over bindings (Map.union (Map.fromList argVars)))
  -- Infer body constraints
  (bt, bc) <- constrain body
  -- Restore bindings
  modify (set bindings oldBindings)
  return (T.Fun (map snd argVars) bt, bc)
constrain (AST.EApp funExpr argExprs nodeId) = do
  (funType, fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argTypes = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret"
  let functionConstraint = T.CEqual (T.Fun argTypes retType) funType nodeId
  return (retType, functionConstraint : fc ++ argConstraints)
constrain (AST.ECase subj branches nodeId) = do
  (subjType, subjConstraints) <- constrain subj
  -- return ()
  _
constrain (AST.EBinary op left right nodeId) = do
  (lType, lConstraints) <- constrain (left)
  (rType, rConstraints) <- constrain right
  resVar <- freshTypeVar "ret"
  let retConstraint = T.CNumeric resVar nodeId
  return (resVar, retConstraint : lConstraints ++ rConstraints)
constrain (AST.ERecord fields _nodeId) = do
  fieldResults <- mapM constrain (snd <$> fields)
  let typedFields = zip fields (fst <$> fieldResults)
      fieldConstraints = concatMap snd fieldResults
  return (T.Record typedFields, fieldConstraints)

constraint (AST.EList elems nodeId) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem"
  let consistencyConstraints = (T.CEqual elemType . fst) <$> elemResults
      elemConstraints = concatMap snd elemResults
  return (T.List elemType, consistencyConstraints ++ elemConstraints)

freshTypeVar :: Text -> Unify T.Type
freshTypeVar name = state (freshTypeVar' name)
  where
    freshTypeVar' n s =
      let newTVId = _nextVarId s
       in (T.Var n newTVId, s {_nextVarId = newTVId + 1})

litType :: AST.Literal -> T.Type
litType (AST.LInt _ _)    = T.Int
litType (AST.LFloat _ _)  = T.Float
litType (AST.LChar _ _)   = T.Char
litType (AST.LString _ _) = T.String

type Subst = [(T.VarId, T.Type)]

-- Unify a set of constraints
unify :: [T.Constraint] -> Either UnificationError Subst
unify [] = Map.empty
unify ((T.CEqual t1 t2 _):cs) | t1 == t2 = unify cs
unify ((T.CEqual (T.Var _ id) t _):cs) = bind id t cs
unify ((T.CEqual t (T.Var _ id) _):cs) = bind id t cs
unify ((T.CEqual t1 t2 nodeId):_) = Left (UnificationError t1 t2 nodeId)
unify ((T.CNumeric T.Float _):cs) = unify cs
unify ((T.CNumeric T.Int _):cs) = unify cs
unify ((T.CNumeric (T.Var _ id) _):cs) = _
unify ((T.CPattern _ _ _):cs)  = _

bind :: T.VarId -> T.Type -> [T.Constraint] -> Either UnificationError Subst
bind id t cs =
  case unify cs of
    Left err -> Left err
    Right subst -> (id, t):subst

-- I think the play here is to use these State CompilerState ... to implement the cache (by allowing them to populate a cache in CompilerState)

valueType :: Text -> Text -> State CompilerState (Either UnificationError T.Type)
valueType modu valName =
  valueDef modu valName
    >>= ( \case
            Right expr -> do
              (inferType, constraints) <- constrain expr
              let subst = unify constraints
              return (applySubst subst inferType)
            Left parseErr -> pure $ Left parseErr
        )

valueDef :: Text -> Text -> State CompilerState (Either UnificationError AST.Expr)
valueDef modu valName = do
  mDef <- moduleDef modu
  return $ mDef >>= findDef valName
  where
    findDef name decls =
      maybe (Left $ UnknownVal name) Right $
        findMaybe
          ( \case
              (AST.ValueDecl n expr _) | n == name -> Just expr
              _ -> Nothing
          )
          decls

constructorFunType :: Text -> Text -> Unify (Either UnificationError T.Type)
constructorFunType modu conName = do
  mDef <- lift (moduleDef modu)
  case mDef of
    Left err    -> return $ Left err
    Right decls -> findConType decls
  where
    findConType decls =
      maybe (Left $ UnknownConstructor conName) Right . listToMaybe . catMaybes
        <$> mapM
          ( \case
              (AST.TypeDecl tName constructors _) -> findConType' tName constructors
              _ -> pure Nothing
          )
          decls
    findConType' tName constructors = do
      listToMaybe . catMaybes
        <$> mapM
          ( \case
              (cName, argTypes) | cName == conName -> Just <$> (T.Fun <$> mapM typify argTypes <*> pure (T.NamedType tName))
              _ -> pure Nothing
          )
          constructors

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

typify :: AST.Type -> Unify T.Type
typify (AST.TInt _) = return T.Int
typify (AST.TFloat _) = return T.Float
typify (AST.TChar _) = return T.Char
typify (AST.TString _) = return T.String
typify (AST.TNamedType name _) = return $ T.NamedType name
typify (AST.TList t _) = T.List <$> typify t
typify (AST.TFun argTypes resType _) = T.Fun <$> mapM typify argTypes <*> typify resType
-- There might be a more elegant way, not sure
typify (AST.TRecord fields _) = T.Record <$> mapM (\(name, aType) -> (name,) <$> typify aType) fields
typify (AST.TVar name _) = freshTypeVar name

-- TODO: Cache module definitions here
moduleDef :: Text -> State CompilerState (Either UnificationError [AST.Declaration])
moduleDef modu = do
  first ParseError <$> P.parse modu
