module MNML.Unification
    ( UnificationError
    , valueType
    ) where

import           Control.Monad.State (State, StateT, evalStateT, gets, lift,
                                      modify, state, runState)
import           Data.Bifunctor      (first, second)
import           Data.Function       (on)
import qualified Data.List as List
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (listToMaybe, mapMaybe, fromMaybe)
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
  | UnknownType Text -- AST.NodeId
  | ArgumentLengthMismatch AST.NodeId
  | UnificationError T.Type T.Type AST.NodeId
  | OccursError T.Type T.Type AST.NodeId
  | ExpectedTraits T.Type [T.Trait] AST.NodeId
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
giveUp err name = addError err >> (,[]) <$> freshTypeVar name []

constrain :: AST.Expr -> Unify (T.Type, [T.Constraint])
constrain (AST.EVar name nodeId) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- The presence of a var doesn't inform it's type
    Just t -> return (t, [])
    Nothing -> do
      addError (UnknownVar nodeId)
      newTVar <- freshTypeVar name []
      return (newTVar, [])
constrain (AST.EConstructor name _nodeId) = do
  modu <- gets _module
  expectedTypeRes <- constructorFunType modu name
  case expectedTypeRes of
    Left err           -> giveUp err "name"
    Right expectedType -> return (expectedType, [])
constrain (AST.ELit lit _nodeId) =
  (, []) <$> litType lit
constrain (AST.ELambda args body nodeId) = do
  oldBindings <- gets _bindings
  -- Fresh argument type bindings
  argVars <- mapM (\arg -> (arg,) <$> freshTypeVar arg []) args
  modify (over bindings (Map.union (Map.fromList argVars)))
  -- Infer body constraints
  (bt, bc) <- constrain body
  -- Restore bindings
  modify (set bindings oldBindings)
  (\v -> (v , T.CEqual v (T.Fun (map snd argVars) bt) nodeId : bc)) <$> freshTypeVar "fun" []
  -- return (T.Fun (map snd argVars) bt, bc)
constrain (AST.EApp funExpr argExprs nodeId) = do
  (funType, fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argTypes = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  let functionConstraint = T.CEqual (T.Fun argTypes retType) funType nodeId
  return (retType, functionConstraint : fc ++ argConstraints)
-- constrain (AST.ECase subj branches _nodeId) = do
--   (subjType, subjConstraints) <- constrain subj
--   -- return ()
-- _

constrain (AST.EBinary _op left right nodeId) = do
  (lType, lConstraints) <- constrain left
  (rType, rConstraints) <- constrain right
  retVar <- freshTypeVar "ret" [T.Numeric]
  let lConstraint = T.CEqual retVar lType nodeId
      rConstraint = T.CEqual retVar rType nodeId
  return (retVar, lConstraint : rConstraint : lConstraints ++ rConstraints)
constrain (AST.ERecord fields _nodeId) = do
  fieldResults <- mapM constrain (snd <$> fields)
  let typedFields = zip (fst <$> fields) (fst <$> fieldResults)
      fieldConstraints = concatMap snd fieldResults
  return (T.Record typedFields, fieldConstraints)
constrain (AST.EList elems nodeId) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem" []
  let consistencyConstraints = (\t -> T.CEqual elemType t nodeId) . fst <$> elemResults
      elemConstraints = concatMap snd elemResults
  return (T.List elemType, consistencyConstraints ++ elemConstraints)

freshTypeVar :: Text -> [T.Trait] -> Unify T.Type
freshTypeVar name traits = state (freshTypeVar' name traits)
  where
    freshTypeVar' n ts s =
      let newTVId = _nextVarId s
       in (T.Var n ts newTVId, s {_nextVarId = newTVId + 1})

litType :: AST.Literal -> Unify T.Type
-- For now, an "int" literal (e.g. "5") will be interpreted as "numeric type" (could be float)
litType (AST.LInt _ _)    = freshTypeVar "num" [T.Numeric]
litType (AST.LFloat _ _)  = return T.Float
litType (AST.LChar _ _)   = return T.Char
litType (AST.LString _ _) = return T.String

type Subst = Map T.Type T.Type

-- Unify a set of constraints

unify :: [T.Constraint] -> State Subst (Maybe UnificationError)
unify [] = return Nothing
-- Delete
unify ((T.CEqual t1 t2 _) : cs) | t1 == t2 = unify cs
-- Decompose
unify ((T.CEqual (T.List a) (T.List b) nodeId) : cs) = unify (T.CEqual a b nodeId : cs)
unify ((T.CEqual (T.Fun argTypes1 retType1) (T.Fun argTypes2 retType2) nodeId) : cs) =
  if length argTypes1 /= length argTypes2
    then return (Just (ArgumentLengthMismatch nodeId))
    else
      let retCon = T.CEqual retType1 retType2 nodeId
          argTypeCons = zipWith (\t1 t2 -> T.CEqual t1 t2 nodeId) argTypes1 argTypes2
       in unify (retCon : (argTypeCons ++ cs))
unify ((T.CEqual (T.Record fieldSpec1) (T.Record fieldSpec2) nodeId) : cs) =
  let commonFields = intersectWith (,) fieldSpec1 fieldSpec2
      fieldConstraints = (\(_, (t1, t2)) -> T.CEqual t1 t2 nodeId) <$> commonFields
   in unify (fieldConstraints ++ cs)
-- Eliminate
-- We don't want to swap incompatible vars back and forth forever, so we try both sides
-- and fail if both sides are incompatible vars
unify ((T.CEqual var1@(T.Var _ traits1 id1) var2@(T.Var _ traits2 id2) nodeId) : cs)
  -- If they're the same, choose the var with the lowest ID
  | List.sort traits1 == List.sort traits2 = if id1 < id2 then bind nodeId var2 var1 cs else bind nodeId var1 var2 cs
  | (var2 `implements`) `all` traits1 = bind nodeId var1 var2 cs
  | (var1 `implements`) `all` traits2 = bind nodeId var2 var1 cs
  | otherwise = return (Just (ExpectedTraits var2 traits1 nodeId))
unify ((T.CEqual var@(T.Var _ traits _) t nodeId) : cs) | (t `implements`) `all` traits = bind nodeId var t cs
-- Swap
unify ((T.CEqual t var@(T.Var _ _ _) nodeId) : cs) = unify (T.CEqual var t nodeId : cs)
-- Conflict
unify ((T.CEqual t1 t2 nodeId) : _) = return (Just (UnificationError t1 t2 nodeId))

implements :: T.Type -> T.Trait -> Bool
implements (T.Var _ traits _) T.Numeric = T.Numeric `elem` traits
implements T.Int T.Numeric = True
implements T.Float T.Numeric = True
implements _ _ = False

intersectWith :: (Ord a) => (b -> b -> c) -> [(a, b)] -> [(a, b)] -> [(a, c)]
intersectWith comb a b = Map.toList $ (Map.intersectionWith comb `on` Map.fromList) a b

bind :: AST.NodeId -> T.Type -> T.Type -> [T.Constraint] -> State Subst (Maybe UnificationError)
bind nodeId var t cs =
  if var `occursIn` t
    then return (Just (OccursError var t nodeId))
    else do
      modify (eliminateAndInsert var t)
      unify ( ( \case
                  (T.CEqual t1 t2 nodeId') -> T.CEqual (applySubst subst t1) (applySubst subst t2) nodeId'
              ) <$> cs)
  where
    subst = (var, t)
    eliminateAndInsert :: T.Type -> T.Type -> Subst -> Subst
    eliminateAndInsert src target = Map.insert src target . Map.map (eliminate src target)
    eliminate :: T.Type -> T.Type -> T.Type -> T.Type
    eliminate src target substType | substType == src = target
    eliminate src target (T.List substElemType) = T.List (eliminate src target substElemType)
    eliminate src target (T.Fun argTypes retType) = T.Fun (eliminate src target <$> argTypes) (eliminate src target retType)
    eliminate src target (T.Record fieldSpec) = T.Record (second (eliminate src target) <$> fieldSpec)
    eliminate _ _ substType = substType

occursIn :: T.Type -> T.Type -> Bool
-- Type aliases currently have to be qualified, so no vars
occursIn _ (T.TypeAlias _) = False
-- Algebraic types currently don't support vars (but will)
occursIn _ (T.AlgebraicType _) = False
occursIn var1 var2 | var1 == var2 = True
occursIn _ (T.Var _ _ _) = False
occursIn _ T.Int = False
occursIn _ T.Float = False
occursIn _ T.Char = False
occursIn _ T.String = False
occursIn var (T.List elemType) = var `occursIn` elemType
occursIn var (T.Fun argTypes retType) = any (var `occursIn`) argTypes || var `occursIn` retType
occursIn var (T.Record fieldSpec) = any ((var `occursIn`) . snd) fieldSpec

applySubst :: (T.Type, T.Type) -> T.Type -> T.Type
applySubst _ (T.TypeAlias name) = T.TypeAlias name
applySubst _ (T.AlgebraicType name) = T.AlgebraicType name
applySubst (var1, rep) var2 | var1 == var2 = rep
applySubst _ var@(T.Var _ _ _) = var
applySubst _ T.Int = T.Int
applySubst _ T.Float = T.Float
applySubst _ T.Char = T.Char
applySubst _ T.String = T.String
applySubst subst (T.List elemType) = T.List (applySubst subst elemType)
applySubst subst (T.Fun argTypes retType) = T.Fun (applySubst subst <$> argTypes) (applySubst subst retType)
applySubst subst (T.Record fieldSpec) = T.Record $ second (applySubst subst) <$> fieldSpec

-- I think the play here is to use these State CompilerState ... to implement the cache (by allowing them to populate a cache in CompilerState)

valueType :: Text -> Text -> State CompilerState (Either UnificationError T.Type)
valueType modu valName = do
  valDef <- valueDef modu valName
  case valDef of
    Right expr ->
      evalStateT
        ( do
            (inferType, constraints) <- constrain expr
            case runState (unify constraints) Map.empty of
              (Just err, _)    -> return (Left err)
              (Nothing, subst) -> return (Right (fromMaybe inferType (subst !? inferType)))
        )
        (initialEnv modu)
    Left parseErr -> return (Left parseErr)

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
    findConType :: [AST.Declaration] -> Unify (Either UnificationError T.Type)
    findConType decls =
      foldl (<>) (Left (UnknownConstructor conName))
        <$> mapM
          ( \case
              (AST.TypeDecl tName constructors _) -> conTypeFromConstructors conName tName constructors
              _ -> (return (Left (UnknownConstructor conName)))
          )
          decls

conTypeFromConstructors :: Text -> Text -> [(Text, [AST.Type])] -> Unify (Either UnificationError T.Type)
conTypeFromConstructors conName tName constructors = do
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (cName, argTypes) | cName == conName -> typifyConstructor argTypes tName
          _ -> return (Left (UnknownConstructor conName))
      )
      constructors

typifyConstructor :: [AST.Type] -> Text -> Unify (Either UnificationError T.Type)
typifyConstructor argTypes tName = do
  typifiedTypes <- mapM typify argTypes
  return $ T.Fun <$> sequence typifiedTypes <*> return (T.AlgebraicType tName)

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

typify :: AST.Type -> Unify (Either UnificationError T.Type)
typify (AST.TInt _) = return (Right T.Int)
typify (AST.TFloat _) = return (Right T.Float)
typify (AST.TChar _) = return (Right T.Char)
typify (AST.TString _) = return (Right T.String)
typify (AST.TNamedType name _) = do
  modName <- gets _module
  moduleNamedType modName name
typify (AST.TList t _) = (T.List <$>) <$> typify t
typify (AST.TFun argTypes resType _) = do
  maybeArgTypes <- mapM typify argTypes
  maybeResType <- typify resType
  return (T.Fun <$> sequence maybeArgTypes <*> maybeResType)
-- There might be a more elegant way, not sure
typify (AST.TRecord fields _) = do
  fieldTypes <- mapM (\(name, aType) -> ((name,) <$>) <$> typify aType) fields
  return $ T.Record <$> sequence fieldTypes
typify (AST.TVar name _) = Right <$> freshTypeVar name []

moduleNamedType :: Text -> Text -> Unify (Either UnificationError T.Type)
moduleNamedType modu typeName = do
  mDefRes <- lift (moduleDef modu)
  case mDefRes of
    Right mDef -> findDef typeName mDef
    Left _     -> return (Left (UnknownType typeName))
  where
    findDef :: Text -> [AST.Declaration] -> Unify (Either UnificationError T.Type)
    findDef name decls =
      foldl (<>) (Left (UnknownType typeName))
        <$> mapM
          ( \case
              (AST.TypeDecl n _ _) | n == name -> return (Right (T.AlgebraicType n))
              (AST.TypeAliasDecl n t _) | n == name -> typify t
              _ -> return (Left (UnknownType typeName))
          )
          decls

-- TODO: Cache module definitions here
moduleDef :: Text -> State CompilerState (Either UnificationError [AST.Declaration])
moduleDef modu = do
  first ParseError <$> P.parse modu
