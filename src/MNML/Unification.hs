module MNML.Unification
    ( UnificationError
    , valueType
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, evalState, get, gets,
                                      modify, put, runState, state)
import           Data.Bifunctor      (first, second)
import           Data.Either         (isLeft)
import           Data.Functor        (($>))
import           Data.Map            (Map, (!), (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over)
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

data UnificationState
  = UnificationState
      { _bindings  :: Map Text T.Type
      , _module    :: Text
      , _nextVarId :: T.VarId
      }

initialEnv :: Text -> UnificationState
initialEnv modu = UnificationState { _bindings = Map.empty
                                   , _module = modu
                                   , _nextVarId = 0
                                   }

bindings :: Lens' UnificationState (Map Text T.Type)
bindings = lens _bindings (\us bin -> us {_bindings = bin})

constrain :: AST.Expr -> Unify [T.Constraint]
constrain = _

exprType' :: AST.Expr -> Unify (Either UnificationError T.Type)

exprType' (AST.EVar name nodeId) = do
  UnificationState {_bindings = b} <- get
  return $ maybe (Left (UnknownVar nodeId)) Right (b !? name)

-- exprType' (AST.EConstructor name nodeId) = do
--   UnificationState{_module = modu} <- get
--   return $ maybe (Left $ UnknownConstructor nodeId) Right (constructorFunType modu name)

exprType' (AST.ELit lit _) = return $ Right $ litType lit

exprType' (AST.ELambda args body _) = do
  populateVariableBindings args
  bodyType <- exprType' body
  finalArgTypes <- gets (\s -> map (_bindings s !) args)
  return $ T.Fun finalArgTypes <$> bodyType

exprType' (AST.EApp fun args nodeId) = do
  funType <- exprType' fun
  case funType of
    Left err -> return $ Left err
    Right (T.Fun argTypes resType) -> if length args > length argTypes then return $ Left (TooManyArguments nodeId)
                                      else if length args < length argTypes then return $ Left (TooFewArguments nodeId)
                                           -- TODO: Unify args and argTypes
                                           else return $ Right resType
    Right otherType -> return $ Left (NotFunctionType otherType nodeId)

-- exprType' (AST.ECase _subj _branches _)    = _

-- Currently just assuming all binary operands are integers
-- exprType' (AST.EBinary _op left right _) =
--   -- This should be essentially the same as a function application
--   _

-- exprType' (AST.ERecord _fields _)          = _

exprType' (AST.EList elems nodeId) = do
  typeVar <- addTypeVar "a"
  foldM (foldType nodeId) (Right typeVar) elems
type Unify a = StateT UnificationState (State CompilerState) a

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

populateVariableBindings :: [Text] -> Unify ()
populateVariableBindings args = do
  typeVars <- mapM (\arg -> (arg,) <$> addTypeVar arg) args
  modify (over bindings (Map.union (Map.fromList typeVars)))

addTypeVar :: Text -> Unify T.Type
addTypeVar name = state (addTypeVar' name)
  where addTypeVar' n s =
          let newTVId = _nextVarId s
          in (T.Var n newTVId, s { _nextVarId = newTVId + 1 })

litType :: AST.Literal -> T.Type
litType (AST.LInt _ _)    =  T.Int
litType (AST.LFloat _ _)  =  T.Float
litType (AST.LChar _ _)   =  T.Char
litType (AST.LString _ _) =  T.String

-- Unify an expression with a type
unify' :: AST.Expr -> T.Type -> Unify (Either UnificationError T.Type)
unify' (AST.EVar name nodeId) t = do
  uniState <- get
  let boundType = _bindings uniState !? name
  case boundType of
    Just t2 -> case unify nodeId t2 t of
      Right uniType -> rebind name uniType $> Right uniType
      Left err      -> return $ Left err
    Nothing -> return $ Left (UnknownVar nodeId)

unify' (AST.EConstructor name nodeId) t = do
  modu <- gets _module
  cLookupRes <- constructorFunType modu name
  return $ cLookupRes >>= (\cType -> unify nodeId cType t)

unify' (AST.ELit lit nodeId) t =
  return $ unify nodeId (litType lit) t

-- unify' ()

rebind :: Text -> T.Type -> Unify ()
rebind name t = modify (over bindings (Map.insert name t))

-- Unify a type to a set of constraints
unify :: T.Type -> T.Type -> Either UnificationError T.Type
unify _ (T.Var _ _) t = Right t
unify _ t (T.Var _ _) = Right t
unify nodeId t1 t2    = Left (UnificationError t1 t2 nodeId)

-- I think the play here is to use these State CompilerState ... to implement the cache (by allowing them to populate a cache in CompilerState)

valueType :: Text -> Text -> Unify (Either UnificationError T.Type)
valueType modu valName =
   valueDef modu valName >>= (\case
                                 Right expr -> addTypeVar valName >>= unify' expr
                                 Left parseErr -> pure $ Left parseErr
                             )

valueDef :: Text -> Text -> Unify (Either UnificationError AST.Expr)
valueDef modu valName = do
  mDef <- moduleDef modu
  return $ mDef >>= findDef valName
  where
    findDef name decls =
      maybe (Left $ UnknownVal name) Right $ findMaybe (\case
                                                           (AST.ValueDecl n expr _) | n == name -> Just expr
                                                           _ -> Nothing
                                                       ) decls

constructorFunType :: Text -> Text -> Unify (Either UnificationError T.Type)
constructorFunType modu conName = do
  mDef <- moduleDef modu
  case mDef of
    Left err    -> return $ Left err
    Right decls -> findConType decls
  where
    findConType decls =
      maybe (Left $ UnknownConstructor conName) Right . listToMaybe . catMaybes <$> mapM (\case
                                                                                             (AST.TypeDecl tName constructors _) -> findConType' tName constructors
                                                                                             _ -> pure Nothing
                                                                                         ) decls
    findConType' tName constructors = do
        listToMaybe . catMaybes <$> mapM (\case
                                             (cName, argTypes) | cName == conName -> Just <$> (T.Fun <$> mapM typify argTypes <*> pure (T.NamedType tName))
                                             _ -> pure Nothing
                                         ) constructors

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

typify :: AST.Type -> Unify T.Type
typify (AST.TInt _)                  = return T.Int
typify (AST.TFloat _)                = return T.Float
typify (AST.TChar _)                 = return T.Char
typify (AST.TString _)               = return T.String
typify (AST.TNamedType name _)       = return $ T.NamedType name
typify (AST.TList t _)               = T.List <$> typify t
typify (AST.TFun argTypes resType _) = T.Fun <$> mapM typify argTypes <*> typify resType
-- There might be a more elegant way, not sure
typify (AST.TRecord fields _)        = T.Record <$> mapM (\(name, aType) -> (name,) <$> typify aType) fields
typify (AST.TVar name _)             = addTypeVar name

-- TODO: Cache module definitions here
moduleDef :: Text -> Unify (Either UnificationError [AST.Declaration])
moduleDef modu = do
  lift (first ParseError <$> P.parse modu)
