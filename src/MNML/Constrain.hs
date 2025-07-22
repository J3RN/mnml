module MNML.Constrain
    ( constrain
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (foldM, mapAndUnzipM)
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (State, StateT, gets, lift, modify,
                                       runStateT)
import           Data.Bifunctor       (bimap, first, second)
import qualified Data.List            as List
import           Data.Map             (Map, (!?))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Debug.Trace          as Debug
import           Lens.Micro           (Lens', lens, over, set)
import           Lens.Micro.Extras    (view)
import qualified MNML.AST.Span        as SAST
import           MNML.AST.Type        (Annotated (..), nodeSpan, nodeType)
import qualified MNML.AST.Type        as TAST
import           MNML.Base            (ModName, QualifiedConstructorReference,
                                       QualifiedTypeReference,
                                       QualifiedValueReference, TypeName,
                                       ValName)
import           MNML.CompilerState   (CompilerState (..), lookupType,
                                       lookupVal, varIdPlusPlus)
import qualified MNML.Constraint      as C
import           MNML.Error           (ConstrainError (..),
                                       Error (ConstrainError), Fallible)
import qualified MNML.Type            as T

-- Local vars, e.g. "x is Int"
type Bindings = Map ValName T.Type

-- Type T is being replaced by Type U
type TypeBindings = Map T.Type T.Type

-- We created a type for "Thing", we should find its real type later
type TempType = (QualifiedTypeReference, T.Type, SAST.SourceSpan)

-- "We created a type for 'doThing', we should find its constraints later."
type TempValueType = (QualifiedValueReference, T.Type, SAST.SourceSpan)

-- Some helpful aliases
type Expr = TAST.Expr TAST.SourceSpanType
type Pattern = TAST.Pattern TAST.SourceSpanType
type Literal = TAST.Literal TAST.SourceSpanType

data ConstrainEnv
  = ConstrainEnv
      { _typeDefs :: Map QualifiedTypeReference TAST.TypeDef
      , _valueDefs :: Map QualifiedValueReference (TAST.ValueDef, [C.Constraint])
      , _bindings :: Bindings
      , _typeBindings :: TypeBindings
      , _definitions :: [SAST.Definition]
      , _errors :: [ConstrainError]
      , _module :: ModName
      , _temporaryTypes :: [TempType]
      , _temporaryValueTypes :: [TempValueType]
      }

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\ce bins -> ce {_bindings = bins})

getBinding :: Text -> ConstrainEnv -> Maybe T.Type
getBinding name = (!? name) . view bindings

typeBindings :: Lens' ConstrainEnv TypeBindings
typeBindings = lens _typeBindings (\ce bins -> ce {_typeBindings = bins})

getTypeBinding :: T.Type -> ConstrainEnv -> Maybe T.Type
getTypeBinding t1 = (!? t1) . view typeBindings

errors :: Lens' ConstrainEnv [ConstrainError]
errors = lens _errors (\ce errs -> ce { _errors = errs } )

addError :: ConstrainError -> Constrain ()
addError err = modify (over errors (err:))

temporaryTypes :: Lens' ConstrainEnv [TempType]
temporaryTypes = lens _temporaryTypes (\ce tt -> ce {_temporaryTypes = tt})

temporaryValueTypes :: Lens' ConstrainEnv [TempValueType]
temporaryValueTypes = lens _temporaryValueTypes (\ce tvt -> ce {_temporaryValueTypes = tvt})

typeDefs :: Lens' ConstrainEnv (Map QualifiedTypeReference TAST.TypeDef)
typeDefs = lens _typeDefs (\ce td -> ce {_typeDefs = td})

getBatchType :: QualifiedTypeReference -> ConstrainEnv -> Maybe T.Type
getBatchType qtr env = (\(TAST.TypeDef t _) -> t) <$> Map.lookup qtr (view typeDefs env)

valueDefs :: Lens' ConstrainEnv (Map QualifiedValueReference (TAST.ValueDef, [C.Constraint]))
valueDefs = lens _valueDefs (\ce te -> ce {_valueDefs = te})

getBatchValueType :: QualifiedValueReference -> ConstrainEnv -> Maybe (T.Type, [C.Constraint])
getBatchValueType qvr env = (\(TAST.ValueDef expr _, cs) -> (nodeType expr, cs)) <$> Map.lookup qvr (view valueDefs env)


initialEnv :: ModName -> SAST.Batch -> ConstrainEnv
initialEnv modu defs  =
  ConstrainEnv
    { _typeDefs = Map.empty
    , _valueDefs = Map.empty
    , _bindings = Map.empty
    , _typeBindings = Map.empty
    , _definitions = defs
    , _errors = []
    , _module = modu
    , _temporaryTypes = []
    , _temporaryValueTypes = []
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

litToLit :: SAST.Literal -> Constrain Literal
litToLit (SAST.LInt int s)      = TAST.LInt int . spanToSpanType s <$> freshTypeVar "num" [T.Numeric]
litToLit (SAST.LFloat double s) = return (TAST.LFloat double (spanToSpanType s T.Float))
litToLit (SAST.LChar char s)    = return (TAST.LChar char (spanToSpanType s T.Char))
litToLit (SAST.LString text s)  = return (TAST.LString text (spanToSpanType s T.String))

giveUp :: ConstrainError -> Text -> Constrain T.Type
giveUp err name = addError err >> freshTypeVar name []

-- The real meat

constrain' :: SAST.Expr -> Constrain (Expr, [C.Constraint])
constrain' (SAST.EVar name spanA) = do
  lookupRes <- gets (getBinding name)
  case lookupRes of
    -- If it's bound, we "know" its type
    Just t -> return (TAST.EVar name (spanToSpanType spanA t), [])
    -- Otherwise, this must be a reference.  Give it a type var and add it to the queue for later.
    Nothing -> do
      -- TODO: Assumes local (same module); need to update to support foreign references
      modu <- gets _module
      newTVar <- freshTypeVar name []
      modify (over temporaryValueTypes (((modu, name), newTVar, spanA) :))
      return (TAST.EVar name (spanToSpanType spanA newTVar), [])
constrain' (SAST.EConstructor name spanA) = do
  -- TODO: Assumes local (same module); need to update to support foreign references
  modu <- gets _module
  let qvr = (modu, name)
  expectedTypeRes <- constructorType qvr
  case expectedTypeRes of
    Nothing -> do
      -- Assumes that we *cannot* find the constructor type
      t <- giveUp (UnknownConstructor qvr spanA) name
      return (TAST.EConstructor name (spanToSpanType spanA t), [])
    Just (expectedType, cs) -> return (TAST.EConstructor name (spanToSpanType spanA expectedType), cs)
constrain' (SAST.ELit lit spanA) = do
  lit' <- litToLit lit
  return (TAST.ELit lit' (spanToSpanType spanA (nodeType lit')), [])
constrain' (SAST.ELambda args body spanA) = do
  (argVars, body', bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (body', bc) <- constrain' body
    return (argVars, body', bc)
  retType <- freshTypeVar "fun" []
  return
    ( TAST.ELambda args body' (spanToSpanType spanA retType)
    , C.CEqual spanA retType (T.Fun argVars (nodeType body')) : bodyConstraints
    )
constrain' (SAST.EApp funExpr argExprs spanA) = do
  (funExpr', fc) <- constrain' funExpr
  argResults <- mapM constrain' argExprs
  let argExprs' = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return
    ( TAST.EApp funExpr' argExprs' (spanToSpanType spanA retType)
    , C.CEqual spanA (T.Fun (map nodeType argExprs') retType) (nodeType funExpr') : fc ++ argConstraints
    )
constrain' (SAST.ECase subj branches spanA) = do
  (subj', subjConstraints) <- constrain' subj
  branches' <- mapM constrainBranch branches
  retType <- freshTypeVar "ret" []
  let (patterns', clauseExprs') = unzip branches'
      -- The subject type will need to match every pattern
      patternConstraints = map (C.CEqual spanA (nodeType subj') . nodeType . fst) patterns'
      patternSubConstraints = concatMap snd patterns'
      -- Every clause type must match the return type of the case expression
      clauseConstraints = map (C.CEqual spanA retType . nodeType . fst) clauseExprs'
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
  let lConstraint = C.CEqual spanA retVar (nodeType left')
      rConstraint = C.CEqual spanA retVar (nodeType right')
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
    , C.CEqual spanA retType (T.Record (Map.fromList (map (second nodeType) fields'))) : fieldConstraints
    )
constrain' (SAST.EList elems spanA) = do
  elemResults <- mapM constrain' elems
  elemType <- freshTypeVar "elem" []
  retType <- freshTypeVar "ret" []
  let consistencyConstraints = map ((\node -> C.CEqual (nodeSpan node) elemType (nodeType node)) . fst) elemResults
      elemConstraints = concatMap snd elemResults
  return
    ( TAST.EList (map fst elemResults) (spanToSpanType spanA retType)
    , C.CEqual spanA (T.List elemType) retType : consistencyConstraints ++ elemConstraints
    )

-- Create constraints based on patterns
constrainPattern :: SAST.Pattern -> Constrain (Pattern, [C.Constraint])
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
      Just (funType@(T.Fun _ _), cs) -> do
        retType <- freshTypeVar name []
        return (retType, C.CEqual spanA (T.Fun (map nodeType argPatterns') retType) funType : cs)
      Just res -> pure res
  return (TAST.PConstructor name argPatterns' (spanToSpanType spanA retType), cons ++ argSubCons)
constrainPattern (SAST.PRecord fieldSpec spanA) = do
  (fieldSpec', fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar "record" []
  partialRecordType <- freshPartialRecord (Map.fromList (map (second nodeType) fieldSpec'))
  return
    ( TAST.PRecord fieldSpec' (spanToSpanType spanA retType)
    , C.CEqual spanA retType partialRecordType : fieldConstraints
    )
  where
    foldRecord :: ([(Text, Pattern)], [C.Constraint]) -> (Text, SAST.Pattern) -> Constrain ([(Text, Pattern)], [C.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (field', fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, field') : fields, fieldSubCons ++ fieldCons)
constrainPattern (SAST.PList elemPats spanA) = do
  (elemPats', elemCons) <- mapAndUnzipM constrainPattern elemPats
  elemType <- freshTypeVar "ret" []
  let retCons = map (C.CEqual spanA elemType . nodeType) elemPats'
  return (TAST.PList elemPats' (spanToSpanType spanA elemType), concat (retCons : elemCons))
constrainPattern (SAST.PLiteral lit spanA) = do
  lit' <- litToLit lit
  return (TAST.PLiteral lit' (spanToSpanType spanA (nodeType lit')), [])

freshTypeVar :: ValName -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> lift varIdPlusPlus

freshAlgebraicType :: TypeName -> Constrain T.Type
freshAlgebraicType name = T.AlgebraicType name <$> lift varIdPlusPlus

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

typify :: SAST.Type -> Constrain T.Type
typify (SAST.TInt _) = return T.Int
typify (SAST.TFloat _) = return T.Float
typify (SAST.TChar _) = return T.Char
typify (SAST.TString _) = return T.String
typify (SAST.TNamedType name spanA) = do
  qtr <- gets ((, name) . _module)
  maybeT <- namedType qtr
  case maybeT of
    (Just t) -> return t
    Nothing  -> do
      -- Create a temp type; put it on the stack to be potentially found later
      t <- freshTypeVar name []
      modify (over temporaryTypes ((qtr, t, spanA):))
      return t
typify (SAST.TList t _) = T.List <$> typify t
typify (SAST.TFun argTypes resType _) = do
  T.Fun <$> mapM typify argTypes <*> typify resType
-- There might be a more elegant way, not sure
typify (SAST.TRecord fields _) = do
  fieldTypes <- mapM (\(fieldName, t) -> (fieldName,) <$> typify t) fields
  return (T.Record (Map.fromList fieldTypes))
-- TODO: We'll probably need the idea of a type binding
typify (SAST.TVar name _) = freshTypeVar name []

-- Defines constructor as a value
-- A constructor is a value whose definition is EConstructor and whose type is T.Fun
defineConstructor :: T.Type -> SAST.Constructor -> Constrain ()
defineConstructor t (SAST.Constructor cName cArgs spanA) = do
  -- TODO: Double check this module fetch
  modName <- gets _module
  cType <- case cArgs of
             -- A constructor without args is considered an instance of its type
             [] -> pure t
             -- A constructor with args is considered a function returning its type
             _  -> (`T.Fun` t) <$> mapM typify cArgs
  let cValDef = TAST.ValueDef (TAST.EConstructor cName (spanToSpanType spanA cType)) spanA
  modify (over valueDefs (Map.insert (modName, cName) (cValDef, [])))

constrain :: SAST.Batch -> Fallible (TAST.Batch, [C.Constraint])
constrain defs = do
  res <- lift (runStateT (extractTypeDefs
                           >> extractTypeAliases
                           >> reconcileTempTypes
                           >> extractValueDefs
                           >> reconcileTempValueTypes) (initialEnv [] defs))
  case res of
    (reconciliationConstraints, env@(ConstrainEnv {_errors = []})) ->
      let valDefs = _valueDefs env in
        return (TAST.Batch { _typeDefs = _typeDefs env
                           , _valueDefs = fst <$> valDefs
                           }
               , concatMap snd (Map.elems valDefs) ++ reconciliationConstraints)

    (_, ConstrainEnv {_errors = errs}) ->
      throwError (map ConstrainError errs)

extractTypeDefs :: Constrain ()
extractTypeDefs = extractDefinitions typeDef
  where
    typeDef (SAST.TypeDef qtr constructors spanA) = do
      t <- freshAlgebraicType (snd qtr)
      mapM_ (defineConstructor t) constructors
      modify (over typeDefs (Map.insert qtr (TAST.TypeDef t spanA)))
    typeDef _ = return ()

extractTypeAliases :: Constrain ()
extractTypeAliases = extractDefinitions typeAliasDef
  where
    typeAliasDef (SAST.TypeAliasDef qtr sastT spanA) = do
      t <- typify sastT
      modify (over typeDefs (Map.insert qtr (TAST.TypeDef t spanA)))
    typeAliasDef _ = return ()

extractValueDefs :: Constrain ()
extractValueDefs = extractDefinitions valueDef
  where
    valueDef (SAST.ValueDef qvr expr spanA) = do
      (typedExpr, cs) <- constrain' expr
      modify (over valueDefs (Map.insert qvr (TAST.ValueDef typedExpr spanA, cs)))
    valueDef _ = return ()

reconcileTempTypes :: Constrain ()
reconcileTempTypes = gets (view temporaryTypes) >>= mapM_ reconcileTempType
  where reconcileTempType :: TempType -> Constrain ()
        reconcileTempType (qtr, tempT, spanA) = do
          maybeT <- namedType qtr
          case maybeT of
            Just realT -> replaceType (tempT, realT)
            Nothing    -> addError (UnknownType qtr spanA)

type Replacement = (T.Type, T.Type)

replaceType :: Replacement -> Constrain ()
replaceType rep = modify (over typeDefs (Map.map (replaceTypeInTypeDef rep))
                         . over valueDefs (Map.map (first (replaceTypeInValueDef rep))))

replaceTypeInTypeDef :: Replacement -> TAST.TypeDef -> TAST.TypeDef
replaceTypeInTypeDef rep (TAST.TypeDef t spanA) = TAST.TypeDef (replaceTypeInType rep t) spanA

-- I think there's probably a cleverer way to write this, but 🤷
replaceTypeInType :: Replacement -> T.Type -> T.Type
replaceTypeInType _ T.Int                     = T.Int
replaceTypeInType _ T.Float                   = T.Float
replaceTypeInType _ T.Char                    = T.Char
replaceTypeInType _ T.String                  = T.String
replaceTypeInType rep (T.List t)              = T.List (replaceTypeInType rep t)
replaceTypeInType rep (T.Fun argTs retT)      = T.Fun (map (replaceTypeInType rep) argTs) (replaceTypeInType rep retT)
replaceTypeInType rep (T.Record fieldSpec)    = T.Record (Map.map (replaceTypeInType rep) fieldSpec)
replaceTypeInType _ t@(T.AlgebraicType _ _)   = t
replaceTypeInType rep (T.TypeAlias alias t)   = T.TypeAlias alias (replaceTypeInType rep t)
replaceTypeInType (tempT, realT) t@(T.Var {}) = if t == tempT then realT else t
replaceTypeInType rep (T.PartialRecord fieldSpec varId) = T.PartialRecord (Map.map (replaceTypeInType rep) fieldSpec) varId

replaceTypeInValueDef :: Replacement -> TAST.ValueDef -> TAST.ValueDef
replaceTypeInValueDef rep (TAST.ValueDef expr spanA) = TAST.ValueDef (replaceTypeInExpr rep expr) spanA

replaceTypeInExpr :: Replacement -> Expr -> Expr
replaceTypeInExpr rep expr = replaceTypeInSourceSpanType <$> expr
  where replaceTypeInSourceSpanType :: TAST.SourceSpanType -> TAST.SourceSpanType
        replaceTypeInSourceSpanType sst = TAST.setType sst (replaceTypeInType rep (TAST.typeOf sst))

reconcileTempValueTypes :: Constrain [C.Constraint]
reconcileTempValueTypes = do
  todos <- gets (view temporaryValueTypes)
  case todos of
    [] -> return []
    _  -> do
      modify (set temporaryValueTypes [])
      cs <- concat <$> mapM reconcilePendingValue (Debug.traceShowId todos)
      additionalCs <- reconcileTempValueTypes
      return ((Debug.traceShowId cs) ++ (Debug.traceShowId additionalCs))
      where reconcilePendingValue :: TempValueType -> Constrain [C.Constraint]
            reconcilePendingValue (qvr, tempT, spanA) = do
              maybeRealTC <- valueType qvr
              case maybeRealTC of
                Just realTC -> do
                  (anonymizedRealT, anonymizedCs) <- anonymizeTypeVars realTC
                  return (C.CEqual spanA tempT anonymizedRealT : anonymizedCs)
                Nothing     -> addError (UnknownValue qvr spanA) >> return []

extractDefinitions :: (SAST.Definition -> Constrain ()) -> Constrain ()
extractDefinitions handler = gets _definitions >>= mapM_ handler

namedType :: QualifiedTypeReference -> Constrain (Maybe T.Type)
namedType qtr = liftA2 (<|>) (gets (getBatchType qtr)) (lift (gets (`lookupType` qtr)))

-- A constructor is a value (function that converts args to a ADT)
constructorType :: QualifiedConstructorReference -> Constrain (Maybe (T.Type, [C.Constraint]))
constructorType = valueType

valueType :: QualifiedValueReference -> Constrain (Maybe (T.Type, [C.Constraint]))
valueType qvr = do
  maybeTC <- liftA2 (<|>) (gets (getBatchValueType qvr)) (lift (gets (\cs -> (, []) . getAnno <$> lookupVal cs qvr)))
  -- There *is* a conciser way to write this, but it's beyond me at the moment
  case maybeTC of
    Nothing -> return Nothing
    Just tc -> Just <$> anonymizeTypeVars tc

anonymizeTypeVars :: (T.Type, [C.Constraint]) -> Constrain (T.Type, [C.Constraint])
anonymizeTypeVars (t, cs) = do modify (set typeBindings Map.empty)
                               t' <- anonymizeTypeVarsInType t
                               cs' <- anonymizeTypeVarsInConstraints cs
                               return (t', cs')

anonymizeTypeVarsInType :: T.Type -> Constrain T.Type
anonymizeTypeVarsInType T.Int = return T.Int
anonymizeTypeVarsInType T.Float = return T.Float
anonymizeTypeVarsInType T.Char = return T.Char
anonymizeTypeVarsInType T.String = return T.String
anonymizeTypeVarsInType (T.List t) = T.List <$> anonymizeTypeVarsInType t
anonymizeTypeVarsInType (T.Fun argTs retT) = T.Fun <$> mapM anonymizeTypeVarsInType argTs <*> anonymizeTypeVarsInType retT
anonymizeTypeVarsInType (T.Record fieldSpec) = T.Record <$> mapM anonymizeTypeVarsInType fieldSpec
anonymizeTypeVarsInType adt@(T.AlgebraicType {}) = return adt
anonymizeTypeVarsInType (T.TypeAlias alias t) = T.TypeAlias alias <$> anonymizeTypeVarsInType t
anonymizeTypeVarsInType var@(T.Var name traits _) = do
  maybeNewVar <- gets (getTypeBinding var)
  case maybeNewVar of
    Just var' -> return var'
    Nothing -> do
      t <- freshTypeVar name (Set.toList traits)
      modify (over typeBindings (Map.insert var t))
      -- If the type being anonymized is a "temp type", the new one is also a "temp type".
      maybeTempType <- gets (List.find(\(_, tempT, _) -> tempT == var) . view temporaryValueTypes)
      case maybeTempType of
        Just (qvr, _tempT, spanA) -> do
          modify (over temporaryValueTypes ((qvr, t, spanA) :))
          return t
        Nothing             -> return t
anonymizeTypeVarsInType (T.PartialRecord fieldSpec varId) = (`T.PartialRecord` varId) <$> traverse anonymizeTypeVarsInType fieldSpec

anonymizeTypeVarsInConstraints :: [C.Constraint] -> Constrain [C.Constraint]
anonymizeTypeVarsInConstraints = mapM anonymizeTypeVarsInConstraint
  where anonymizeTypeVarsInConstraint :: C.Constraint -> Constrain C.Constraint
        anonymizeTypeVarsInConstraint (C.CEqual spanA t1 t2) = do
          t1' <- anonymizeTypeVarsInType t1
          t2' <- anonymizeTypeVarsInType t2
          return (C.CEqual spanA t1' t2')
