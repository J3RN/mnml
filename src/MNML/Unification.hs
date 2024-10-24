module MNML.Unification
    ( UnificationError
    , valueType
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, get, gets, lift, modify,
                                      put, runState, state)
import           Data.Bifunctor      (first, second)
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, listToMaybe, mapMaybe)
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..))
import qualified MNML.AST            as AST
import qualified MNML.Parser         as P
import qualified MNML.Type           as T

data UnificationError
  = UnknownVar AST.NodeId
  | UnknownVal Text
  | UnknownConstructor Text
  | ArgumentLengthMismatch AST.NodeId
  | UnificationError T.Type T.Type AST.NodeId
  | OccursError T.Type T.Type AST.NodeId
  | ExpectedNumeric T.Type AST.NodeId
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
constrain (AST.ELambda args body _nodeId) = do
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
constrain (AST.ECase subj branches _nodeId) = do
  (subjType, subjConstraints) <- constrain subj
  -- return ()
  _
constrain (AST.EBinary _op left right nodeId) = do
  (lType, lConstraints) <- constrain left
  (rType, rConstraints) <- constrain right
  resVar <- freshTypeVar "ret"
  let retConstraint = T.CEqual T.Numeric resVar nodeId
      lConstraint = T.CEqual T.Numeric lType nodeId
      rConstraint = T.CEqual T.Numeric rType nodeId
  return (resVar, retConstraint : lConstraint : rConstraint : lConstraints ++ rConstraints)
constrain (AST.ERecord fields _nodeId) = do
  fieldResults <- mapM constrain (snd <$> fields)
  let typedFields = zip (fst <$> fields) (fst <$> fieldResults)
      fieldConstraints = concatMap snd fieldResults
  return (T.Record typedFields, fieldConstraints)

constrain (AST.EList elems nodeId) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem"
  let consistencyConstraints = (\t -> T.CEqual elemType t nodeId) . fst <$> elemResults
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

type Subst = Map T.VarId T.Type

-- Unify a set of constraints

unify :: [T.Constraint] -> Either UnificationError Subst
unify [] = Right Map.empty
-- Delete rule
unify ((T.CEqual t1 t2 _):cs) | t1 == t2 = unify cs
-- Decompose rules
unify ((T.CEqual (T.List a) (T.List b) nodeId):cs) = unify (T.CEqual a b nodeId:cs)
unify ((T.CEqual (T.Fun argTypes1 retType1) (T.Fun argTypes2 retType2) nodeId):cs) =
  if length argTypes1 /= length argTypes2
  then Left (ArgumentLengthMismatch nodeId)
  else let retCon = T.CEqual retType1 retType2 nodeId
           argTypeCons = zipWith (\t1 t2 -> T.CEqual t1 t2 nodeId) argTypes1 argTypes2
       in unify (retCon:(argTypeCons ++ cs))
unify ((T.CEqual (T.Record fieldSpec1) (T.Record fieldSpec2) nodeId):cs) =
  let commonFields = intersectWith (,) fieldSpec1 fieldSpec2
      fieldConstraints = (\(_, (t1, t2)) -> T.CEqual t1 t2 nodeId) <$> commonFields
  in unify (fieldConstraints ++ cs)
-- Swap
unify ((T.CEqual t var@(T.Var _ _) nodeId):cs) = unify (T.CEqual var t nodeId:cs)
-- Numeric swap
unify ((T.CEqual t T.Numeric nodeId):cs) = unify (T.CEqual T.Numeric t nodeId:cs)
-- Eliminate
unify ((T.CEqual var@(T.Var _ _) t nodeId):cs) = bind nodeId var t cs
-- Numeric elimination
unify ((T.CEqual T.Numeric T.Float _):cs) = unify cs
unify ((T.CEqual T.Numeric T.Int _):cs) = unify cs
-- Conflict
unify ((T.CEqual t1 t2 nodeId):_) = Left (UnificationError t1 t2 nodeId)
-- unify ((T.CPattern _ _ _):cs)  = _

intersectWith :: (b -> b -> c) -> [(a, b)] -> [(a, b)] -> [(a, c)]
intersectWith comb a b = Map.toList $ (Map.intersectionWith comb `on` Map.fromList) a b

bind :: AST.NodeId -> T.Type -> T.Type -> [T.Constraint] -> Either UnificationError Subst
bind nodeId var t cs =
  if var `occursIn` t
  then Left (OccursError var t nodeId)
  else unify $ (\case
                 (T.CEqual t1 t2 nodeId') -> T.CEqual (applySubst subst t1) (applySubst subst t2) nodeId'
             ) <$> cs
  where subst = (var, t)

occursIn :: T.Type -> T.Type -> Bool
-- Type aliases currently have to be qualified, so no vars
occursIn _ (T.TypeAlias _) = False
-- Algebraic types currently don't support vars (but will)
occursIn _ (T.AlgebraicType _) = False
occursIn var1 var2 | var1 == var2 = True
occursIn _ (T.Var _ _) = False
occursIn _ T.Int    = False
occursIn _ T.Float  = False
occursIn _ T.Char   = False
occursIn _ T.String = False
occursIn var (T.List elemType) = var `occursIn` elemType
occursIn var (T.Fun argTypes retType) = any (var `occursIn`) argTypes || var `occursIn` retType
occursIn var (T.Record fieldSpec) = any ((var `occursIn`) . snd) fieldSpec

applySubst :: (T.Type, T.Type) -> T.Type -> T.Type
applySubst _ (T.TypeAlias name) = T.TypeAlias name
applySubst _ (T.AlgebraicType name) = T.AlgebraicType name
applySubst (var1, rep) var2 | var1 == var2 = rep
applySubst _ var@(T.Var _ _) = var
applySubst _ T.Int = T.Int
applySubst _ T.Float = T.Float
applySubst _ T.Char = T.Char
applySubst _ T.String = T.String
applySubst subst (T.List elemType) = T.List (applySubst subst elemType)
applySubst subst (T.Fun argTypes retType) = T.Fun (applySubst subst <$> argTypes) (applySubst subst retType)
applySubst subst (T.Record fieldSpec) = T.Record $ second (applySubst subst) <$> fieldSpec

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
              (cName, argTypes) | cName == conName -> Just <$> (T.Fun <$> mapM typify argTypes <*> pure (T.AlgebraicType tName))
              _ -> pure Nothing
          )
          constructors

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

typify :: AST.Type -> Unify (Either UnificationError T.Type)
typify (AST.TInt _) = return (Right T.Int)
typify (AST.TFloat _) = return (Right T.Float)
typify (AST.TChar _) = return (Right T.Char)
typify (AST.TString _) = return (Right T.String)
typify (AST.TNamedType name _) = do
  maybeT <- lift (moduleNamedType name)
  case maybeT of
    Just t  -> return (Right t)
    Nothing -> _
typify (AST.TList t _) = (T.List <$>) <$> typify t
typify (AST.TFun argTypes resType _) = do
  maybeArgTypes <- mapM typify argTypes
  maybeResType <- typify resType
  return (T.Fun <$> sequence maybeArgTypes <*> maybeResType)
-- There might be a more elegant way, not sure
typify (AST.TRecord fields _) = Right . T.Record <$> mapM (\(name, aType) -> (name,) <$> typify aType) fields
typify (AST.TVar name _) = Right <$> freshTypeVar name

moduleNamedType :: Text -> State CompilerState (Maybe T.Type)
moduleNamedType name = gets (_)

-- TODO: Cache module definitions here
moduleDef :: Text -> State CompilerState (Either UnificationError [AST.Declaration])
moduleDef modu = do
  first ParseError <$> P.parse modu
