module MNML.Constrain
    ( ConstrainRes (..)
    , ConstraintError
    , valueConstraints
    ) where

import           Control.Monad       (foldM, mapAndUnzipM)
import           Control.Monad.State (State, StateT, gets, lift, modify,
                                      runStateT)
import           Data.Bifunctor      (bimap, second)
import           Data.Functor        (($>))
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..), varIdPlusPlus)
import           MNML.AST.Span       (spanOf)
import qualified MNML.AST.Span       as SAST
import           MNML.AST.Type       (typeOf)
import qualified MNML.AST.Type       as TAST
import           MNML.Base           (QualifiedValueReference)
import qualified MNML.Constraint     as C
import qualified MNML.Parse          as P
import qualified MNML.Type           as T

type Bindings = Map Text T.Type

type PendingType = (QualifiedValueReference, T.Type, SAST.SourceSpan)

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

spanToSpanType :: SAST.SourceSpan -> T.Type -> TAST.SourceSpanType
spanToSpanType (SAST.SourceSpan {_spanStart = s, _spanEnd = e}) t =
  TAST.SourceSpanType
    { _spanStart = s
    , _spanEnd = e
    , _type = t
    }

opToOp :: SAST.Operator -> TAST.Operator
opToOp SAST.Add    = TAST.Add
opToOp SAST.Sub    = TAST.Sub
opToOp SAST.Mul    = TAST.Mul
opToOp SAST.Div    = TAST.Div
opToOp SAST.And    = TAST.And
opToOp SAST.Or     = TAST.Or
opToOp SAST.Equals = TAST.Equals

litToLit :: SAST.Literal -> Constrain TAST.Literal
litToLit (SAST.LInt int s)      = TAST.LInt int . spanToSpanType s <$> freshTypeVar "num" [T.Numeric]
litToLit (SAST.LFloat double s) = return (TAST.LFloat double (spanToSpanType s T.Float))
litToLit (SAST.LChar char s)    = return (TAST.LChar char (spanToSpanType s T.Char))
litToLit (SAST.LString text s)  = return (TAST.LString text (spanToSpanType s T.String))

addError :: ConstraintError -> Constrain ()
addError err = modify (\s -> s {_errors = err : _errors s})

giveUp :: ConstraintError -> Text -> Constrain T.Type
giveUp err name = addError err >> freshTypeVar name []

-- The real meat

constrain :: SAST.Expr -> Constrain (TAST.Expr, [C.Constraint])
constrain (SAST.EVar name spanA) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- If it's bound, we "know" its type
    Just t -> return (TAST.EVar name (spanToSpanType spanA t), [])
    -- Otherwise, this must be a reference.  Give it a type var and add it to the queue for later.
    Nothing -> do
      newTVar <- freshTypeVar name []
      modu <- gets _module
      modify (over pendingTypes (((modu, name), newTVar, spanA) :))
      return (TAST.EVar name (spanToSpanType spanA newTVar), [])
constrain (SAST.EConstructor name spanA) = do
  modu <- gets _module
  expectedTypeRes <- constructorType modu name
  case expectedTypeRes of
    Left err -> do
      t <- giveUp err name
      return (TAST.EConstructor name (spanToSpanType spanA t), [])
    Right expectedType -> return (TAST.EConstructor name (spanToSpanType spanA expectedType), [])
constrain (SAST.ELit lit spanA) = do
  lit' <- litToLit lit
  return (TAST.ELit lit' (spanToSpanType spanA (typeOf lit')), [])
constrain (SAST.ELambda args body spanA) = do
  (argVars, body', bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (body', bc) <- constrain body
    return (argVars, body', bc)
  retType <- freshTypeVar "fun" []
  return
    ( TAST.ELambda args body' (spanToSpanType spanA retType)
    , C.CEqual spanA retType (T.Fun argVars (typeOf body')) : bodyConstraints
    )
constrain (SAST.EApp funExpr argExprs spanA) = do
  (funExpr', fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argExprs' = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return
    ( TAST.EApp funExpr' argExprs' (spanToSpanType spanA retType)
    , C.CEqual spanA (T.Fun (map typeOf argExprs') retType) (typeOf funExpr') : fc ++ argConstraints
    )
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
    ( TAST.ECase subj' (map (bimap fst fst) branches') (spanToSpanType spanA retType)
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
  return
    ( TAST.EBinary (opToOp op) left' right' (spanToSpanType spanA retVar)
    , lConstraint : rConstraint : lConstraints ++ rConstraints
    )
constrain (SAST.ERecord fields spanA) = do
  fieldVals' <- mapM (constrain . snd) fields
  let fields' = zip (map fst fields) (map fst fieldVals')
      fieldConstraints = concatMap snd fieldVals'
  retType <- freshTypeVar "ret" []
  return
    ( TAST.ERecord fields' (spanToSpanType spanA retType)
    , C.CEqual spanA retType (T.Record (Map.fromList (map (second typeOf) fields'))) : fieldConstraints
    )
constrain (SAST.EList elems spanA) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem" []
  retType <- freshTypeVar "ret" []
  let consistencyConstraints = map ((\node -> C.CEqual (spanOf node) elemType (typeOf node)) . fst) elemResults
      elemConstraints = concatMap snd elemResults
  return
    ( TAST.EList (map fst elemResults) (spanToSpanType spanA retType)
    , C.CEqual spanA (T.List elemType) retType : consistencyConstraints ++ elemConstraints
    )

-- Create constraints based on patterns
constrainPattern :: SAST.Pattern -> Constrain (TAST.Pattern, [C.Constraint])
constrainPattern (SAST.PVar t spanA) = do
  varType <- declareVar t
  return (TAST.PVar t (spanToSpanType spanA varType), [])
constrainPattern (SAST.PDiscard spanA) = do
  varType <- freshTypeVar "_" []
  return (TAST.PDiscard (spanToSpanType spanA varType), [])
constrainPattern (SAST.PConstructor name argPatterns spanA) = do
  modu <- gets _module
  argCons <- mapM constrainPattern argPatterns
  let argPatterns' = map fst argCons
      argSubCons = concatMap snd argCons
  funTypeRes <- constructorType modu name
  (retType, cons) <-
    case funTypeRes of
      Left err -> (,[]) <$> giveUp err name
      Right funType@(T.Fun _ _) -> do
        retType <- freshTypeVar name []
        return (retType, [C.CEqual spanA (T.Fun (map typeOf argPatterns') retType) funType])
      Right t -> pure (t, [])
  return (TAST.PConstructor name argPatterns' (spanToSpanType spanA retType), cons ++ argSubCons)
constrainPattern (SAST.PRecord fieldSpec spanA) = do
  (fieldSpec', fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar "record" []
  partialRecordType <- freshPartialRecord (Map.fromList (map (second typeOf) fieldSpec'))
  return
    ( TAST.PRecord fieldSpec' (spanToSpanType spanA retType)
    , C.CEqual spanA retType partialRecordType : fieldConstraints
    )
  where
    foldRecord ::
      ([(Text, TAST.Pattern)], [C.Constraint]) ->
      (Text, SAST.Pattern) ->
      Constrain ([(Text, TAST.Pattern)], [C.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (field', fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, field') : fields, fieldSubCons ++ fieldCons)
constrainPattern (SAST.PList elemPats spanA) = do
  (elemPats', elemCons) <- mapAndUnzipM constrainPattern elemPats
  elemType <- freshTypeVar "ret" []
  let retCons = map (C.CEqual spanA elemType . typeOf) elemPats'
  return (TAST.PList elemPats' (spanToSpanType spanA elemType), concat (retCons : elemCons))
constrainPattern (SAST.PLiteral lit spanA) = do
  lit' <- litToLit lit
  return (TAST.PLiteral lit' (spanToSpanType spanA (typeOf lit')), [])

freshTypeVar :: Text -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> lift varIdPlusPlus

freshPartialRecord :: T.FieldSpec -> Constrain T.Type
freshPartialRecord fields = T.PartialRecord fields <$> lift varIdPlusPlus

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

constructorType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
constructorType modu conName = do
  mDef <- lift (P.moduleDef modu)
  case mDef of
    Left err   -> return (Left (ParseError err))
    Right defs -> findConType defs conName

findConType :: [SAST.Definition] -> Text -> Constrain (Either ConstraintError T.Type)
findConType defs conName =
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (SAST.TypeDef tName constructors _) -> conTypeFromConstructors conName tName constructors
          _ -> return (Left (UnknownConstructor conName))
      )
      defs

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
  fieldTypes <- mapM (\(fieldName, t) -> ((fieldName,) <$>) <$> typify t) fields
  return (T.Record . Map.fromList <$> sequence fieldTypes)
typify (SAST.TVar name _) = Right <$> freshTypeVar name []

moduleNamedType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
moduleNamedType modu typeName = do
  mDefRes <- lift (P.moduleDef modu)
  case mDefRes of
    Right mDef -> findDef typeName mDef
    Left _     -> return (Left (UnknownType typeName))
  where
    findDef :: Text -> [SAST.Definition] -> Constrain (Either ConstraintError T.Type)
    findDef name defs =
      foldl (<>) (Left (UnknownType typeName))
        <$> mapM
          ( \case
              (SAST.TypeDef n _ _) | n == name -> return (Right (T.AlgebraicType n))
              (SAST.TypeAliasDef n t _) | n == name -> (T.TypeAlias n <$>) <$> typify t
              _ -> return (Left (UnknownType typeName))
          )
          defs

data ConstrainRes
  = ConstrainRes
      { _typedExpr     :: TAST.Expr
      , _constraints   :: [C.Constraint]
        -- UGH
      , _pendingTypes' :: [(QualifiedValueReference, T.Type, SAST.SourceSpan)]
      }

valueConstraints ::
  QualifiedValueReference -> State CompilerState (Either [ConstraintError] ConstrainRes)
valueConstraints qvr@(modu, _) = do
  res <$> runStateT (valueConstraints' qvr) (initialEnv modu)
  where
    valueConstraints' :: QualifiedValueReference -> Constrain (Maybe (TAST.Expr, [C.Constraint]))
    valueConstraints' qvr' = do
      def <- lift (P.valueDef qvr')
      case def of
        Right expr -> Just <$> constrain expr
        Left err   -> addError (ParseError err) $> Nothing
    res :: (Maybe (TAST.Expr, [C.Constraint]), ConstrainEnv) -> Either [ConstraintError] ConstrainRes
    res (Just (t, cs), ConstrainEnv {_errors = [], _pendingTypes = pt}) = Right (ConstrainRes {_typedExpr = t, _constraints = cs, _pendingTypes' = pt})
    res (_, ConstrainEnv {_errors = errs}) = Left errs
