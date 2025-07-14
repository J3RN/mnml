module MNML.Constrain
    ( constrain
    ) where

import           Control.Monad        (foldM, mapAndUnzipM)
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (State, StateT, execStateT, gets, lift,
                                       modify)
import           Data.Bifunctor       (bimap, second)
import           Data.Map             (Map, (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Lens.Micro           (Lens', lens, over, set)
import           Lens.Micro.Extras    (view)
import           MNML.AST.Span        (spanOf)
import qualified MNML.AST.Span        as SAST
import           MNML.AST.Type        (typeOf)
import qualified MNML.AST.Type        as TAST
import           MNML.Base            (ModName, QualifiedConstructorReference,
                                       QualifiedTypeReference,
                                       QualifiedValueReference, ValName)
import           MNML.CompilerState   (CompilerState (..), lookupType,
                                       lookupVal, varIdPlusPlus)
import qualified MNML.Constraint      as C
import           MNML.Error           (ConstrainError (..),
                                       Error (ConstrainError), Fallible)
import qualified MNML.Type            as T

-- Local vars, e.g. "x is Int"
type Bindings = Map ValName T.Type

-- "We don't know what type 'doThing' is, let's find out later"
type PendingType = (QualifiedValueReference, T.Type, SAST.SourceSpan)

data ConstrainEnv
  = ConstrainEnv
      { _batch        :: TAST.Batch
      , _bindings     :: Bindings
      , _constraints' :: [C.Constraint]
      , _definitions  :: [SAST.Definition]
      , _errors       :: [ConstrainError]
      , _module       :: ModName
      , _pendingTypes :: [PendingType]
      }

batch :: Lens' ConstrainEnv TAST.Batch
batch = lens _batch (\ce bin -> ce {_batch = bin})

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\ce bin -> ce {_bindings = bin})

constraints :: Lens' ConstrainEnv [C.Constraint]
constraints = lens _constraints' (\ce cs -> ce {_constraints' = cs})

errors :: Lens' ConstrainEnv [ConstrainError]
errors = lens _errors (\ce errs -> ce { _errors = errs } )

pendingTypes :: Lens' ConstrainEnv [PendingType]
pendingTypes = lens _pendingTypes (\ce pt -> ce {_pendingTypes = pt})

initialEnv :: ModName -> SAST.Batch -> ConstrainEnv
initialEnv modu defs  =
  ConstrainEnv
    { _batch = TAST.Batch { _typeDefs = Map.empty, _typeAliasDefs = Map.empty, _valueDefs = Map.empty}
    , _bindings = Map.empty
    , _constraints' = []
    , _definitions = defs
    , _errors = []
    , _module = modu
    , _pendingTypes = []
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

addError :: ConstrainError -> Constrain ()
addError err = modify (over errors (err:))

giveUp :: ConstrainError -> Text -> Constrain T.Type
giveUp err name = addError err >> freshTypeVar name []

-- The real meat

constrain' :: SAST.Expr -> Constrain (TAST.Expr, [C.Constraint])
constrain' (SAST.EVar name spanA) = do
  lookupRes <- gets ((!? name) . view bindings)
  case lookupRes of
    -- If it's bound, we "know" its type
    Just t -> return (TAST.EVar name (spanToSpanType spanA t), [])
    -- Otherwise, this must be a reference.  Give it a type var and add it to the queue for later.
    Nothing -> do
      newTVar <- freshTypeVar name []
      -- Assume local (same module)
      modu <- gets _module
      modify (over pendingTypes (((modu, name), newTVar, spanA) :))
      return (TAST.EVar name (spanToSpanType spanA newTVar), [])
constrain' (SAST.EConstructor name spanA) = do
  modu <- gets _module
  let qvr = (modu, name)
  expectedTypeRes <- constructorType qvr
  case expectedTypeRes of
    Nothing -> do
      t <- giveUp (UnknownConstructor qvr spanA) name
      return (TAST.EConstructor name (spanToSpanType spanA t), [])
    Just expectedType -> return (TAST.EConstructor name (spanToSpanType spanA expectedType), [])
constrain' (SAST.ELit lit spanA) = do
  lit' <- litToLit lit
  return (TAST.ELit lit' (spanToSpanType spanA (typeOf lit')), [])
constrain' (SAST.ELambda args body spanA) = do
  (argVars, body', bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (body', bc) <- constrain' body
    return (argVars, body', bc)
  retType <- freshTypeVar "fun" []
  return
    ( TAST.ELambda args body' (spanToSpanType spanA retType)
    , C.CEqual spanA retType (T.Fun argVars (typeOf body')) : bodyConstraints
    )
constrain' (SAST.EApp funExpr argExprs spanA) = do
  (funExpr', fc) <- constrain' funExpr
  argResults <- mapM constrain' argExprs
  let argExprs' = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return
    ( TAST.EApp funExpr' argExprs' (spanToSpanType spanA retType)
    , C.CEqual spanA (T.Fun (map typeOf argExprs') retType) (typeOf funExpr') : fc ++ argConstraints
    )
constrain' (SAST.ECase subj branches spanA) = do
  (subj', subjConstraints) <- constrain' subj
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
      bExpr' <- constrain' bExpr
      return (patternConstraints, bExpr')
constrain' (SAST.EBinary op left right spanA) = do
  (left', lConstraints) <- constrain' left
  (right', rConstraints) <- constrain' right
  retVar <- freshTypeVar "ret" [T.Numeric]
  let lConstraint = C.CEqual spanA retVar (typeOf left')
      rConstraint = C.CEqual spanA retVar (typeOf right')
  return
    ( TAST.EBinary (opToOp op) left' right' (spanToSpanType spanA retVar)
    , lConstraint : rConstraint : lConstraints ++ rConstraints
    )
constrain' (SAST.ERecord fields spanA) = do
  fieldVals' <- mapM (constrain' . snd) fields
  let fields' = zip (map fst fields) (map fst fieldVals')
      fieldConstraints = concatMap snd fieldVals'
  retType <- freshTypeVar "ret" []
  return
    ( TAST.ERecord fields' (spanToSpanType spanA retType)
    , C.CEqual spanA retType (T.Record (Map.fromList (map (second typeOf) fields'))) : fieldConstraints
    )
constrain' (SAST.EList elems spanA) = do
  elemResults <- mapM constrain' elems
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
      qvr = (modu, name)
  funTypeRes <- constructorType qvr
  (retType, cons) <-
    case funTypeRes of
      Nothing -> (,[]) <$> giveUp (UnknownConstructor qvr spanA) name
      Just funType@(T.Fun _ _) -> do
        retType <- freshTypeVar name []
        return (retType, [C.CEqual spanA (T.Fun (map typeOf argPatterns') retType) funType])
      Just t -> pure (t, [])
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

freshTypeVar :: ValName -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> lift varIdPlusPlus

freshPartialRecord :: T.FieldSpec -> Constrain T.Type
freshPartialRecord fields = T.PartialRecord fields <$> lift varIdPlusPlus

-- Runs a function within its own scope (inheriting the existing scope)
withNewScope :: Constrain a -> Constrain a
withNewScope f = do
  oldBindings <- gets (view bindings)
  result <- f
  modify (set bindings oldBindings)
  return result

declareVar :: ValName -> Constrain T.Type
declareVar name = do
  newVarType <- freshTypeVar name []
  modify (over bindings (Map.insert name newVarType))
  return newVarType

constructorType :: QualifiedConstructorReference -> Constrain (Maybe T.Type)
constructorType qvr = do
  -- Step 1: See if it's in the data being loaded
  -- TODO
  -- Step 2: See if it's in the store
  (typeOf <$>) <$> lift (gets (`lookupVal` qvr))

typify :: SAST.Type -> Constrain (Maybe T.Type)
typify (SAST.TInt _) = return (Just T.Int)
typify (SAST.TFloat _) = return (Just T.Float)
typify (SAST.TChar _) = return (Just T.Char)
typify (SAST.TString _) = return (Just T.String)
typify (SAST.TNamedType name _) = do
  modName <- gets _module
  moduleNamedType (modName, name)
typify (SAST.TList t _) = (T.List <$>) <$> typify t
typify (SAST.TFun argTypes resType _) = do
  maybeArgTypes <- mapM typify argTypes
  maybeResType <- typify resType
  return (T.Fun <$> sequence maybeArgTypes <*> maybeResType)
-- There might be a more elegant way, not sure
typify (SAST.TRecord fields _) = do
  fieldTypes <- mapM (\(fieldName, t) -> ((fieldName,) <$>) <$> typify t) fields
  return (T.Record . Map.fromList <$> sequence fieldTypes)
typify (SAST.TVar name _) = Just <$> freshTypeVar name []

moduleNamedType :: QualifiedTypeReference -> Constrain (Maybe T.Type)
moduleNamedType qvr = do
  -- Step 1: See if it's in the data being loaded
  -- TODO
  -- Step 2: See if it's in the store
  lift (gets (`lookupType` qvr))

-- TODO: We need pending types in the case of a circular dependence, e.g. (haskell syntax)
-- data Foo = Foo Bar
-- data Bar = Bar Foo
convertConstructor :: SAST.Constructor -> Constrain TAST.Constructor
convertConstructor (SAST.Constructor cName cArgs spanA) = do
  newArgs <- mapM convertType cArgs
  return (TAST.Constructor cName newArgs spanA)
  where convertType :: SAST.Type -> Constrain (T.Type, SAST.SourceSpan)
        convertType sast = do
          maybeT <- typify sast
          modName <- gets _module
          t <- fromMaybe <$> giveUp (UnknownType (modName, nameOf sast) (spanOf sast)) (Text.pack (show sast)) <*> pure maybeT
          return (t, spanOf sast)
        nameOf :: SAST.Type -> Text
        nameOf (SAST.TNamedType name _) = name
        -- Really, this clause should never be hit
        nameOf sast                     = Text.pack (show sast)

constrain :: SAST.Batch -> Fallible (TAST.Batch, [C.Constraint])
constrain defs = do
  res <- lift (execStateT (extractTypeDefs >> extractTypeAliases >> extractValueDefs) (initialEnv [] defs))
  case res of
    env@(ConstrainEnv {_errors = []}) ->
      return (_batch env, _constraints' env)

    (ConstrainEnv {_errors = errs}) ->
      throwError (map ConstrainError errs)

extractTypeDefs :: Constrain ()
extractTypeDefs = extractDefinitions typeDef
  where
    typeDef (SAST.TypeDef qtr constructors spanA) = do
      constructors' <- mapM convertConstructor constructors
      modify (over (batch . TAST.typeDefs) (Map.insert qtr (TAST.TypeDef constructors' spanA)))
    typeDef _ = return ()

extractTypeAliases :: Constrain ()
extractTypeAliases = extractDefinitions typeAliasDef
  where
    typeAliasDef (SAST.TypeAliasDef qtr sastT spanA) = do
      maybeType <- typify sastT
      case maybeType of
        Just t  -> modify (over (batch . TAST.typeAliasDefs) (Map.insert qtr (TAST.TypeAliasDef t spanA)))
        Nothing -> addError (UnknownType qtr spanA)
    typeAliasDef _ = return ()

extractValueDefs :: Constrain ()
extractValueDefs = extractDefinitions valueDef
  where
    valueDef (SAST.ValueDef qvr expr spanA) = do
      (typedExpr, cs) <- constrain' expr
      modify (over (batch . TAST.valueDefs) (Map.insert qvr typedExpr) . over constraints (++ cs))
    valueDef _ = return ()

extractDefinitions :: (SAST.Definition -> Constrain ()) -> Constrain ()
extractDefinitions handler = gets _definitions >>= mapM_ handler
