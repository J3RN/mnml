module MNML.Constrain
    ( ConstraintError
    , TypedValueDecl
    , valueConstraints
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, gets, lift, modify,
                                      runStateT)
import           Data.Bifunctor      (bimap, second)
import           Data.Functor        (($>))
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..),
                                      QualifiedValueReference)
import qualified MNML.Constraint     as C
import qualified MNML.Parse          as P
import qualified MNML.SAST.Span      as SAST
import qualified MNML.SAST.Type      as TAST
import qualified MNML.Type           as T

type Bindings = Map Text T.Type

type PendingType = (QualifiedValueReference, T.Type, SAST.SourceSpan)

type TypedValueDecl = (QualifiedValueReference, SAST.Expr)

data ConstraintError
  = UnknownConstructor Text -- SAST.SourceSpan
  | UnknownType Text -- SAST.SourceSpan
  | ParseError P.ParseError
  deriving (Eq, Show)

data ConstrainEnv
  = ConstrainEnv
      { _bindings     :: Bindings
      , _module       :: Text
      , _pendingTypes :: [PendingType]
      , _errors       :: [ConstraintError]
      }

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\us bin -> us {_bindings = bin})

pendingTypes :: Lens' ConstrainEnv [PendingType]
pendingTypes = lens _pendingTypes (\us pt -> us {_pendingTypes = pt})

initialEnv :: Text -> ConstrainEnv
initialEnv modu =
  ConstrainEnv
    { _bindings = Map.empty
    , _module = modu
    , _pendingTypes = []
    , _errors = []
    }

type Constrain a = StateT ConstrainEnv (State CompilerState) a

-- Helpers

addError :: ConstraintError -> Constrain ()
addError err = modify (\s -> s {_errors = err : _errors s})

-- We need something like "typeable"->"typed"
giveUp :: ConstraintError -> Text -> SAST.Expr -> Constrain (TAST.Expr, [C.Constraint])
giveUp err name expr = do
  _ <- addError err
  tVar <- freshTypeVar name []
  return (spanToSpanType tVar <$> expr, [])

-- The real meat

constrain :: SAST.Expr -> Constrain (TAST.Expr, [C.Constraint])
constrain (SAST.EVar name spanA) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- The presence of a var doesn't inform it's type
    Just t -> return (SAST.EVar name (spanToSpanType t spanA), [])
    Nothing -> do
      newTVar <- freshTypeVar name []
      modu <- gets _module
      modify (over pendingTypes (((modu, name), newTVar, spanA) :))
      return (SAST.EVar name (spanToSpanType newTVar spanA), [])
constrain cons@(SAST.EConstructor name spanA) = do
  modu <- gets _module
  expectedTypeRes <- constructorType modu name
  case expectedTypeRes of
    Left err           -> giveUp err name cons
    Right expectedType -> return (SAST.EConstructor name (spanToSpanType expectedType spanA), [])
constrain (SAST.ELit l spanA) = do
  lType <- litType l
  return (SAST.ELit (spanToSpanType lType <$> l) (spanToSpanType lType spanA), [])
constrain (SAST.ELambda args body spanA) = do
  (argVars, body', bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (body', bc) <- constrain body
    return (argVars, body', bc)
  retType <- freshTypeVar "fun" []
  return (SAST.ELambda args body' (spanToSpanType retType spanA)
         , C.CEqual spanA retType (T.Fun argVars (typeOf body')) : bodyConstraints)
constrain (SAST.EApp funExpr argExprs spanA) = do
  (funExpr', fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argExprs' = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return ( SAST.EApp funExpr' argExprs' (spanToSpanType retType spanA)
         , C.CEqual spanA (T.Fun (map typeOf argExprs') retType) (typeOf funExpr') : fc ++ argConstraints)
constrain (SAST.ECase subj branches spanA) = do
  (subj', subjConstraints) <- constrain subj
  branches' <- mapM constrainBranch branches
  retType <- freshTypeVar "ret" []
  let (patterns', clauseExprs') = unzip branches'
      -- The subject type will need to match every pattern
      patternConstraints = map (C.CEqual spanA (typeOf subj') . typeOf . fst) patterns'
      patternSubConstraints = concatMap snd patterns'
      -- Every clause type must match the return type of the case expression
      clauseConstraints = map (C.CEqual spanA retType . typeOf . fst) clauseExprs'
      clauseSubConstraints = concatMap snd clauseExprs'
  return
    ( SAST.ECase subj' (map (bimap fst fst) branches') (spanToSpanType retType spanA)
    , concat
        [ patternConstraints
        , clauseConstraints
        , subjConstraints
        , patternSubConstraints
        , clauseSubConstraints
        ]
    )
  where
    constrainBranch (bPattern, bExpr) = withNewScope $ do
      patternConstraints <- constrainPattern bPattern
      bExpr' <- constrain bExpr
      return (patternConstraints, bExpr')
constrain (SAST.EBinary op left right spanA) = do
  (left', lConstraints) <- constrain left
  (right', rConstraints) <- constrain right
  retVar <- freshTypeVar "ret" [T.Numeric]
  let lConstraint = C.CEqual spanA retVar (typeOf left')
      rConstraint = C.CEqual spanA retVar (typeOf right')
  return ( SAST.EBinary op left' right' (spanToSpanType retVar spanA)
         , lConstraint : rConstraint : lConstraints ++ rConstraints)
constrain (SAST.ERecord fields spanA) = do
  fieldVals' <- mapM (constrain . snd) fields
  let fields' = zip (map fst fields) (map fst fieldVals')
      fieldConstraints = concatMap snd fieldVals'
  retType <- freshTypeVar "ret" []
  return ( SAST.ERecord fields' (spanToSpanType retType spanA)
         , C.CEqual spanA retType (T.Record (Map.fromList (map (second typeOf) fields'))) : fieldConstraints)
constrain (SAST.EList elems spanA) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem" []
  retType <- freshTypeVar "ret" []
  let consistencyConstraints = map (C.CEqual spanA elemType . typeOf . fst) elemResults
      elemConstraints = concatMap snd elemResults
  return ( SAST.EList (map fst elemResults) (spanToSpanType retType spanA)
         , C.CEqual spanA (T.List elemType) retType : consistencyConstraints ++ elemConstraints)

-- Create constraints based on patterns
constrainPattern :: SpanPattern -> Constrain (SpanTypePattern, [C.Constraint])
constrainPattern (SAST.PVar t spanA) = (,[]) . SAST.PVar t <$> liftA2 spanToSpanType (declareVar t) (pure spanA)
constrainPattern (SAST.PDiscard spanA) = (,[]) . SAST.PDiscard <$> liftA2 spanToSpanType (freshTypeVar "_" []) (pure spanA)
constrainPattern (SAST.PConstructor name argPatterns spanA) = do
  modu <- gets _module
  funTypeRes <- constructorType modu name
  case funTypeRes of
    Left err -> giveUp err name
    Right funType@(T.Fun _ _) -> do
      retType <- freshTypeVar name []
      argCons <- mapM constrainPattern argPatterns
      let argTypes = map fst argCons
          argSubCons = concatMap snd argCons
      return (retType, C.CEqual spanA (T.Fun argTypes retType) funType : argSubCons)
    Right retType -> return (retType, [])
constrainPattern (SAST.PRecord fieldSpec spanA) = do
  (fieldTypes, fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar "record" []
  partialRecordType <- freshPartialRecord (Map.fromList fieldTypes)
  return (retType, C.CEqual spanA retType partialRecordType : fieldConstraints)
  where
    -- foldRecord ::
    --   ([(Text, T.Type)], [C.Constraint]) ->
    --   (Text, SAST.Pattern ) ->
    --   Constrain ([(Text, T.Type)], [C.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (fieldType, fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, fieldType) : fields, fieldSubCons ++ fieldCons)
constrainPattern (SAST.PList elemPats spanA) = do
  elemCons <- mapM constrainPattern elemPats
  elemType <- freshTypeVar "ret" []
  let retCons = map (C.CEqual spanA elemType . fst) elemCons
      elemSubCons = concatMap snd elemCons
  return (elemType, retCons ++ elemSubCons)
constrainPattern (SAST.PLiteral lit _) = (,[]) <$> litType lit

freshTypeVar :: Text -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> varIdPlusPlus

freshPartialRecord :: T.FieldSpec -> Constrain T.Type
freshPartialRecord fields = T.PartialRecord fields <$> varIdPlusPlus

-- Runs a function within its own scope (inheriting the existing scope)
withNewScope :: Constrain a -> Constrain a
withNewScope f = do
  oldBindings <- gets _bindings
  result <- f
  modify (set bindings oldBindings)
  return result

declareVar :: Text -> Constrain T.Type
declareVar name = do
  newVarType <- freshTypeVar name []
  modify (over bindings (Map.insert name newVarType))
  return newVarType

litType :: SAST.Literal -> Constrain T.Type
-- For now, an "int" literal (e.g. "5") will be interpreted as "numeric type" (could be float)
litType (SAST.LInt _ _)    = freshTypeVar "num" [T.Numeric]
litType (SAST.LFloat _ _)  = return T.Float
litType (SAST.LChar _ _)   = return T.Char
litType (SAST.LString _ _) = return T.String

constructorType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
constructorType modu conName = do
  mDef <- lift (P.moduleDef modu)
  case mDef of
    Left err    -> return (Left (ParseError err))
    Right decls -> findConType decls conName

findConType :: [SpanDecl] -> Text -> Constrain (Either ConstraintError T.Type)
findConType decls conName =
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (SAST.TypeDecl tName constructors _) -> conTypeFromConstructors conName tName constructors
          _ -> return (Left (UnknownConstructor conName))
      )
      decls

conTypeFromConstructors ::
  Text -> Text -> [(Text, [SAST.Type])] -> Constrain (Either ConstraintError T.Type)
conTypeFromConstructors conName tName constructors = do
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (cName, argTypes) | cName == conName -> typifyConstructor argTypes tName
          _ -> return (Left (UnknownConstructor conName))
      )
      constructors

typifyConstructor :: [SAST.Type] -> Text -> Constrain (Either ConstraintError T.Type)
typifyConstructor [] tName = return (Right (T.AlgebraicType tName))
typifyConstructor argTypes tName = do
  typifiedTypes <- mapM typify argTypes
  return (T.Fun <$> sequence typifiedTypes <*> return (T.AlgebraicType tName))

typify :: SAST.Type -> Constrain (Either ConstraintError T.Type)
typify (SAST.TInt _) = return (Right T.Int)
typify (SAST.TFloat _) = return (Right T.Float)
typify (SAST.TChar _) = return (Right T.Char)
typify (SAST.TString _) = return (Right T.String)
typify (SAST.TNamedType name _) = do
  modName <- gets _module
  moduleNamedType modName name
typify (SAST.TList t _) = (T.List <$>) <$> typify t
typify (SAST.TFun argTypes resType _) = do
  maybeArgTypes <- mapM typify argTypes
  maybeResType <- typify resType
  return (T.Fun <$> sequence maybeArgTypes <*> maybeResType)
-- There might be a more elegant way, not sure
typify (SAST.TRecord fields _) = do
  fieldTypes <- mapM (\(fieldName, SAST.ype) -> ((fieldName,) <$>) <$> typify SAST.ype) fields
  return (T.Record . Map.fromList <$> sequence fieldTypes)
typify (SAST.TVar name _) = Right <$> freshTypeVar name []

moduleNamedType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
moduleNamedType modu typeName = do
  mDefRes <- lift (P.moduleDef modu)
  case mDefRes of
    Right mDef -> findDef typeName mDef
    Left _     -> return (Left (UnknownType typeName))
  where
    findDef :: Text -> [SpanDecl] -> Constrain (Either ConstraintError T.Type)
    findDef name decls =
      foldl (<>) (Left (UnknownType typeName))
        <$> mapM
          ( \case
              (SAST.TypeDecl n _ _) | n == name -> return (Right (T.AlgebraicType n))
              (SAST.TypeAliasDecl n t _) | n == name -> (T.TypeAlias n <$>) <$> typify t
              _ -> return (Left (UnknownType typeName))
          )
          decls

valueConstraints :: QualifiedValueReference -> State CompilerState (Either [ConstraintError] ([TypedValueDecl], [C.Constraint]))
valueConstraints qvr@(modu, _) = do
  res <$> runStateT (valueConstraints' qvr) (initialEnv modu)
  where
    res :: (([TypedValueDecl], [C.Constraint]), ConstrainEnv) -> Either [ConstraintError] ([TypedValueDecl], [C.Constraint])
    res (t, ConstrainEnv {_errors = []})   = Right t
    res (_, ConstrainEnv {_errors = errs}) = Left errs

valueConstraints' :: QualifiedValueReference -> Constrain ([TypedValueDecl], [C.Constraint])
valueConstraints' qvr@(modu, valName) = do
  def <- lift (P.valueDef qvr)
  case def of
    Right expr -> do
      modify (\s -> s {_module = modu, _pendingTypes = []})
      (resT, cs) <- constrain expr
      pTypes <- gets _pendingTypes
      -- TODO: WARNING: This is a trick for circular references but *only works in the same module*
      modify (\s -> s {_bindings = Map.insert valName resT (_bindings s)})
      foldM foldPendingType ([(qvr, (\(SpanAnnotation s) -> SpanTypeAnnotation resT s) <$> expr)], cs) pTypes
    Left err -> addError (ParseError err) $> ([], [])
  where
    foldPendingType :: ([TypedValueDecl], [C.Constraint]) -> PendingType -> Constrain ([TypedValueDecl], [C.Constraint])
    foldPendingType (tvds, cs) (qvr', t, spanA) = do
      (tvds', cs') <- valueConstraints' qvr'
      case tvds' of
        []       -> return (tvds ++ tvds', cs ++ cs')
        (_, e):_ -> let t' = typeOf e
                     in return (tvds ++ tvds', C.CEqual spanA t t' : cs ++ cs')
