module MNML.Constrain
    ( constrain
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (foldM, mapAndUnzipM)
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (State, StateT, evalStateT, gets, lift,
                                       modify, runStateT)
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Data.Bifunctor       (first, second)
import qualified Data.List            as List
import           Data.Map             (Map, (!?))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
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

-- Some helpful aliases
type Expr = TAST.Expr TAST.SourceSpanType
type Pattern = TAST.Pattern TAST.SourceSpanType
type Literal = TAST.Literal TAST.SourceSpanType

-- "We created a type for 'doThing', we should find its constraints later."
type TempValueType = (QualifiedValueReference, T.Type, SAST.SourceSpan)

-- The "whole picture" of a Value Definition
-- The AST, its constrants, its temporary value types
type ValDef' = (TAST.ValueDef, [C.Constraint], [TempValueType])

data Batch
  = Batch
      { _typeDefs  :: Map QualifiedTypeReference TAST.TypeDef
      , _valueDefs :: Map QualifiedValueReference ValDef'
      }
  deriving (Eq, Show)

typeDefs :: Lens' Batch (Map QualifiedTypeReference TAST.TypeDef)
typeDefs = lens _typeDefs (\ce td -> ce {_typeDefs = td})

valueDefs :: Lens' Batch (Map QualifiedValueReference ValDef')
valueDefs = lens _valueDefs (\ce vds -> ce {_valueDefs = vds})

data BatchEnv
  = BatchEnv
      { _batch          :: Batch
      , _errors         :: [ConstrainError]
      , _temporaryTypes :: [TempType]
      , _typeBindings   :: TypeBindings
      }

batch :: Lens' BatchEnv Batch
batch = lens _batch (\be b -> be { _batch = b })

getBatchType :: QualifiedTypeReference -> BatchEnv -> Maybe T.Type
getBatchType qtr env = typ <$> Map.lookup qtr (view (batch . typeDefs) env)
  where typ (TAST.TypeDef t _) = t

defineBatchType :: QualifiedTypeReference -> TAST.TypeDef -> Batch' ()
defineBatchType qtr td = modify (over (batch . typeDefs) (Map.insert qtr td))

defineBatchValue :: QualifiedValueReference -> ValDef' -> Batch' ()
defineBatchValue qvr v = modify (over (batch . valueDefs) (Map.insert qvr v))

getBatchValueType :: QualifiedValueReference -> BatchEnv -> Maybe  (T.Type, [C.Constraint], [TempValueType])
getBatchValueType qvr env = typ <$> Map.lookup qvr (view (batch . valueDefs) env)
  where typ (TAST.ValueDef expr _, cs, tvts) = (nodeType expr, cs, tvts)

errors :: Lens' BatchEnv [ConstrainError]
errors = lens _errors (\ce errs -> ce { _errors = errs } )

addError :: ConstrainError -> Batch' ()
addError err = modify (over errors (err:))

temporaryTypes :: Lens' BatchEnv [TempType]
temporaryTypes = lens _temporaryTypes (\ce tt -> ce {_temporaryTypes = tt})

typeBindings :: Lens' BatchEnv TypeBindings
typeBindings = lens _typeBindings (\ce bins -> ce {_typeBindings = bins})

getTypeBinding :: T.Type -> BatchEnv -> Maybe T.Type
getTypeBinding t1 = (!? t1) . view typeBindings

initialEnv :: BatchEnv
initialEnv =
  BatchEnv
    { _batch = Batch {_typeDefs = Map.empty, _valueDefs = Map.empty}
    , _typeBindings = Map.empty
    , _errors = []
    , _temporaryTypes = []
    }

data ConstrainRes
  = ConstrainRes
      { _constraints         :: [C.Constraint]
      , _temporaryValueTypes :: [TempValueType]
      }

instance Semigroup ConstrainRes where
  a <> b = ConstrainRes { _constraints = _constraints a ++ _constraints b
                        , _temporaryValueTypes = _temporaryValueTypes a ++ _temporaryValueTypes b}

instance Monoid ConstrainRes where
  mempty = ConstrainRes { _constraints = [], _temporaryValueTypes = []}

constraints :: Lens' ConstrainRes [C.Constraint]
constraints = lens _constraints (\ce cs -> ce { _constraints = cs })

addConstraints :: [C.Constraint] -> Constrain' ()
addConstraints cs = tell (ConstrainRes { _constraints = cs, _temporaryValueTypes = [] })

temporaryValueTypes :: Lens' ConstrainRes [TempValueType]
temporaryValueTypes = lens _temporaryValueTypes (\ce tvt -> ce {_temporaryValueTypes = tvt})

addTemporaryValueType :: TempValueType -> Constrain' ()
addTemporaryValueType tvt = tell (ConstrainRes { _constraints = [], _temporaryValueTypes = [tvt] })

data ConstrainEnv
  = ConstrainEnv
      { _bindings :: Bindings
      , _module   :: ModName
      }

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\ce bins -> ce {_bindings = bins})

getBinding :: Text -> ConstrainEnv -> Maybe T.Type
getBinding name = (!? name) . view bindings

type Batch' = StateT BatchEnv (State CompilerState)
type Constrain' = WriterT ConstrainRes (StateT ConstrainEnv Batch')

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

litToLit :: SAST.Literal ->  Constrain' Literal
litToLit (SAST.LInt int s)      = TAST.LInt int . spanToSpanType s <$> freshTypeVar' "num" [T.Numeric]
litToLit (SAST.LFloat double s) = return (TAST.LFloat double (spanToSpanType s T.Float))
litToLit (SAST.LChar char s)    = return (TAST.LChar char (spanToSpanType s T.Char))
litToLit (SAST.LString text s)  = return (TAST.LString text (spanToSpanType s T.String))

giveUp :: ConstrainError -> Text -> Constrain' T.Type
giveUp err name = lift (lift (addError err >> freshTypeVar name []))

-- The real meat

constrain :: SAST.Batch -> Fallible Batch
constrain defs = do
  res <- lift (runStateT (extractTypeDefs defs
                          >> extractTypeAliases defs
                          >> reconcileTempTypes
                          >> extractValueDefs defs) initialEnv)
  case res of
    ((), BatchEnv {_batch = finalBatch, _errors = []}) -> return finalBatch
    ((), BatchEnv {_errors = errs}) -> throwError (map ConstrainError errs)

constrain' :: SAST.Expr -> Constrain' Expr
constrain' (SAST.EVar name spanA) = do
  lookupRes <- gets (getBinding name)
  case lookupRes of
    -- If it's bound, we "know" its type
    Just t -> return (TAST.EVar name (spanToSpanType spanA t))
    -- Otherwise, this must be a reference.  Give it a type var and add it to the queue for later.
    Nothing -> do
      -- TODO: Assumes local (same module); need to update to support foreign references
      modu <- gets _module
      newTVar <- lift (lift (freshTypeVar name []))
      addTemporaryValueType ((modu, name), newTVar, spanA)
      return (TAST.EVar name (spanToSpanType spanA newTVar))
constrain' (SAST.EConstructor name spanA) = do
  -- TODO: Assumes local (same module); need to update to support foreign references
  modu <- gets _module
  let qvr = (modu, name)
  expectedTypeRes <- lift (constructorType qvr)
  case expectedTypeRes of
    Nothing -> do
      -- Assumes that we *cannot* find the constructor type
      t <- giveUp (UnknownConstructor qvr spanA) name
      return (TAST.EConstructor name (spanToSpanType spanA t))
    Just (expectedType, cs) ->
      addConstraints cs >> return (TAST.EConstructor name (spanToSpanType spanA expectedType))
constrain' (SAST.ELit lit spanA) = do
  lit' <- litToLit lit
  return (TAST.ELit lit' (spanToSpanType spanA (nodeType lit')))
constrain' (SAST.ELambda args body spanA) = do
  (argVars, body') <- withNewScope $ do
    argVars <- mapM declareVar args
    body' <- constrain' body
    return (argVars, body')
  retType <- freshTypeVar' "fun" []
  addConstraints [C.CEqual spanA retType (T.Fun argVars (nodeType body'))]
  return (TAST.ELambda args body' (spanToSpanType spanA retType))
constrain' (SAST.EApp funExpr argExprs spanA) = do
  funExpr' <- constrain' funExpr
  argExprs' <- mapM constrain' argExprs
  retType <- freshTypeVar' "ret" []
  addConstraints [C.CEqual spanA (T.Fun (map nodeType argExprs') retType) (nodeType funExpr')]
  return (TAST.EApp funExpr' argExprs' (spanToSpanType spanA retType))
constrain' (SAST.ECase subj branches spanA) = do
  subj' <- constrain' subj
  branches' <- mapM constrainBranch branches
  retType <- freshTypeVar' "ret" []
  let (patterns', clauseExprs') = unzip branches'
      -- The subject type will need to match every pattern
      patternConstraints = map (C.CEqual spanA (nodeType subj') . nodeType) patterns'
      -- Every clause type must match the return type of the case expression
      clauseConstraints = map (C.CEqual spanA retType . nodeType) clauseExprs'
  addConstraints (patternConstraints ++ clauseConstraints)
  return (TAST.ECase subj' branches' (spanToSpanType spanA retType))
  where
    constrainBranch :: (SAST.Pattern, SAST.Expr) -> Constrain' (Pattern, Expr)
    constrainBranch (bPattern, bExpr) = withNewScope $ do
      (pattern, cs) <- constrainPattern bPattern
      addConstraints cs
      bExpr' <- constrain' bExpr
      return (pattern, bExpr')
constrain' (SAST.EBinary op left right spanA) = do
  left' <- constrain' left
  right' <- constrain' right
  retVar <- freshTypeVar' "ret" [T.Numeric]
  addConstraints [ C.CEqual spanA retVar (nodeType left')
                 , C.CEqual spanA retVar (nodeType right')]
  return (TAST.EBinary (opToOp op) left' right' (spanToSpanType spanA retVar))
constrain' (SAST.ERecord fields spanA) = do
  fieldVals' <- mapM (constrain' . snd) fields
  let fields' = zip (map fst fields) fieldVals'
  retType <-  freshTypeVar' "ret" []
  addConstraints [C.CEqual spanA retType (T.Record (Map.fromList (map (second nodeType) fields')))]
  return (TAST.ERecord fields' (spanToSpanType spanA retType))
constrain' (SAST.EList elems spanA) = do
  elemResults <- mapM constrain' elems
  elemType <- freshTypeVar' "elem" []
  retType <- freshTypeVar' "ret" []
  let consistencyConstraints = map (\listElem -> C.CEqual (nodeSpan listElem) elemType (nodeType listElem)) elemResults
  addConstraints (C.CEqual spanA (T.List elemType) retType : consistencyConstraints)
  return (TAST.EList elemResults (spanToSpanType spanA retType))

-- Create constraints based on patterns
-- TODO: Should this add constraints directly to the state instead of returning them?
constrainPattern :: SAST.Pattern -> Constrain' (Pattern, [C.Constraint])
constrainPattern (SAST.PVar t spanA) = do
  varType <- declareVar t
  return (TAST.PVar t (spanToSpanType spanA varType), [])
constrainPattern (SAST.PDiscard spanA) = do
  varType <- freshTypeVar' "_" []
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
        retType <- freshTypeVar' name []
        return (retType, C.CEqual spanA (T.Fun (map nodeType argPatterns') retType) funType : cs)
      Just res -> pure res
  return (TAST.PConstructor name argPatterns' (spanToSpanType spanA retType), cons ++ argSubCons)
constrainPattern (SAST.PRecord fieldSpec spanA) = do
  (fieldSpec', fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar' "record" []
  partialRecordType <- freshPartialRecord (Map.fromList (map (second nodeType) fieldSpec'))
  return
    ( TAST.PRecord fieldSpec' (spanToSpanType spanA retType)
    , C.CEqual spanA retType partialRecordType : fieldConstraints
    )
  where
    foldRecord :: ([(Text, Pattern)], [C.Constraint]) -> (Text, SAST.Pattern) -> Constrain' ([(Text, Pattern)], [C.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (field', fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, field') : fields, fieldSubCons ++ fieldCons)
constrainPattern (SAST.PList elemPats spanA) = do
  (elemPats', elemCons) <- mapAndUnzipM constrainPattern elemPats
  elemType <- freshTypeVar' "ret" []
  let retCons = map (C.CEqual spanA elemType . nodeType) elemPats'
  return (TAST.PList elemPats' (spanToSpanType spanA elemType), concat (retCons : elemCons))
constrainPattern (SAST.PLiteral lit spanA) = do
  lit' <- litToLit lit
  return (TAST.PLiteral lit' (spanToSpanType spanA (nodeType lit')), [])

freshTypeVar :: ValName -> [T.Trait] -> Batch' T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> lift varIdPlusPlus

freshTypeVar' :: ValName -> [T.Trait] -> Constrain' T.Type
freshTypeVar' name traits = lift (lift (freshTypeVar name traits))

freshAlgebraicType :: TypeName -> Batch' T.Type
freshAlgebraicType name = T.AlgebraicType name <$> lift varIdPlusPlus

freshPartialRecord :: T.FieldSpec -> Constrain' T.Type
freshPartialRecord fields = T.PartialRecord fields <$> lift (lift (lift varIdPlusPlus))

-- Runs a function within its own scope (inheriting the existing scope)
withNewScope :: Constrain' a -> Constrain' a
withNewScope f = do
  oldBindings <- gets (view bindings)
  result <- f
  modify (set bindings oldBindings)
  return result

declareVar :: ValName -> Constrain' T.Type
declareVar name = do
  newVarType <- freshTypeVar' name []
  modify (over bindings (Map.insert name newVarType))
  return newVarType

typify :: SAST.Type -> Batch' T.Type
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
      addTemporaryValueType (qtr, t, spanA)
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
defineConstructor :: ModName -> T.Type -> SAST.Constructor -> Batch' ()
defineConstructor modName t (SAST.Constructor cName cArgs spanA) = do
  cType <- case cArgs of
             -- A constructor without args is considered an instance of its type
             [] -> pure t
             -- A constructor with args is considered a function returning its type
             _  -> (`T.Fun` t) <$> mapM typify cArgs
  let cValDef = TAST.ValueDef (TAST.EConstructor cName (spanToSpanType spanA cType)) spanA
  defineBatchValue (modName, cName) (cValDef, [], [])

extractTypeDefs :: SAST.Batch -> Batch' ()
extractTypeDefs = mapM_ typeDef
  where
    typeDef (SAST.TypeDef qtr@(modName, cName) constructors spanA) = do
      t <- freshAlgebraicType cName
      defineBatchType qtr (TAST.TypeDef t spanA)
      mapM_ (defineConstructor modName t) constructors
    typeDef _ = return ()

extractTypeAliases :: SAST.Batch -> Batch' ()
extractTypeAliases = mapM_ typeAliasDef
  where
    typeAliasDef (SAST.TypeAliasDef qtr sastT spanA) = do
      t <- typify sastT
      defineBatchType qtr (TAST.TypeDef t spanA)
    typeAliasDef _ = return ()

extractValueDefs :: SAST.Batch -> Batch' ()
extractValueDefs = mapM_ valueDef
  where
    valueDef (SAST.ValueDef qvr@(modName, _valName) expr spanA) = do
      (typedExpr, ConstrainRes { _constraints = cs, _temporaryValueTypes = tvts}) <- evalStateT (runWriterT (constrain' expr)) (ConstrainEnv { _module = modName, _bindings = Map.empty})
      defineBatchValue qvr (TAST.ValueDef typedExpr spanA, cs, tvts)
    valueDef _ = return ()

-- For each temporary type that was defined, we find its "real" type.
reconcileTempTypes :: Batch' ()
reconcileTempTypes = gets (view temporaryTypes) >>= mapM_ reconcileTempType
  where reconcileTempType :: TempType -> Batch' ()
        reconcileTempType (qtr, tempT, spanA) = do
          maybeT <- namedType qtr
          case maybeT of
            -- If we find its "real" type, replace all instances of the temporary type with the "real" type.
            Just realT -> replaceType (tempT, realT)
            -- If we *cannot* find its "real" type, mark this is an error.
            Nothing    -> addError (UnknownType qtr spanA)

type Replacement = (T.Type, T.Type)

replaceType :: Replacement -> Batch' ()
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

namedType :: QualifiedTypeReference -> Batch' (Maybe T.Type)
namedType qtr = do
  maybeBatchType <- gets (getBatchType qtr)
  maybeSavedType <- lift (gets (`lookupType` qtr))
  return (maybeBatchType <|> maybeSavedType)

-- A constructor is a value (function that converts args to a ADT)
constructorType :: QualifiedConstructorReference -> Batch' (Maybe (T.Type, [C.Constraint]))
constructorType = valueType

valueType :: QualifiedValueReference -> Batch' (Maybe (T.Type, [C.Constraint]))
valueType qvr = do
  batchType <- gets (getBatchValueType qvr)
  savedType <- lift (gets (\cs -> (, [], []) . getAnno <$> lookupVal cs qvr))
  -- There *is* a conciser way to write this, but it's beyond me at the moment
  case batchType <|> savedType of
    Nothing              -> return Nothing
    Just (t, cs, _temps) -> Just <$> anonymizeTypeVars (t, cs)

anonymizeTypeVars :: (T.Type, [C.Constraint]) -> Batch' (T.Type, [C.Constraint])
anonymizeTypeVars (t, cs) = do modify (set typeBindings Map.empty)
                               t' <- anonymizeTypeVarsInType t
                               cs' <- anonymizeTypeVarsInConstraints cs
                               return (t', cs')

anonymizeTypeVarsInType :: T.Type -> Batch' T.Type
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
      maybeTempType <- gets (List.find (\(_, tempT, _) -> tempT == var) . view temporaryValueTypes)
      case maybeTempType of
        Just (qvr, _tempT, spanA) -> do
          modify (over temporaryValueTypes ((qvr, t, spanA) :))
          return t
        Nothing             -> return t
anonymizeTypeVarsInType (T.PartialRecord fieldSpec varId) = (`T.PartialRecord` varId) <$> traverse anonymizeTypeVarsInType fieldSpec

anonymizeTypeVarsInConstraints :: [C.Constraint] -> Batch' [C.Constraint]
anonymizeTypeVarsInConstraints = mapM anonymizeTypeVarsInConstraint
  where anonymizeTypeVarsInConstraint :: C.Constraint -> Batch' C.Constraint
        anonymizeTypeVarsInConstraint (C.CEqual spanA t1 t2) = do
          t1' <- anonymizeTypeVarsInType t1
          t2' <- anonymizeTypeVarsInType t2
          return (C.CEqual spanA t1' t2')
