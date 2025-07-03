module MNML.Unify
    ( valueType
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, evalStateT, gets, lift,
                                      modify, runStateT)
import           Data.Bifunctor      (bimap, second)
import           Data.Function       (on)
import           Data.Functor        (($>))
import qualified Data.List           as List
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified MNML.AST.Span       as SAST
import qualified MNML.AST.Type       as TAST
import           MNML.Base           (QualifiedReference)
import           MNML.CompilerState  (CompilerState (..), varIdPlusPlus)
import qualified MNML.Constrain      as C
import           MNML.Constraint     (Constraint (..))
import           MNML.Error          (Error (UnificationError))
import qualified MNML.Type           as T

type Subst = Map T.Type T.Type

type Constrain = StateT Subst (State CompilerState)

-- Unify a set of constraints

unify :: [Constraint] -> Constrain (Maybe UnificationError)
unify [] = return Nothing
-- Delete
unify ((CEqual _ t1 t2) : cs) | t1 == t2 = unify cs
-- Decompose
unify ((CEqual sSpan (T.List a) (T.List b)) : cs) = unify (CEqual sSpan a b : cs)
unify ((CEqual sSpan (T.Fun argTypes1 retType1) (T.Fun argTypes2 retType2)) : cs) =
  if length argTypes1 /= length argTypes2
    then return (Just (ArgumentLengthMismatch sSpan))
    else
      let retCon = CEqual sSpan retType1 retType2
          argTypeCons = zipWith (CEqual sSpan) argTypes1 argTypes2
       in unify (retCon : (argTypeCons ++ cs))
unify ((CEqual sSpan rec1@(T.Record fieldSpec1) rec2@(T.Record fieldSpec2)) : cs) =
  if List.sort (Map.keys fieldSpec1) == List.sort (Map.keys fieldSpec2)
    then unify (commonFieldConstraints sSpan fieldSpec1 fieldSpec2 ++ cs)
    else return (Just (UnificationError rec1 rec2 sSpan))
-- Eliminate
unify ((CEqual sSpan var1@(T.Var _ traits1 id1) var2@(T.Var _ traits2 id2)) : cs)
  -- Try to reuse a type var if possible
  | traits1 == traits2 =
      if id1 <= id2
        then bind' sSpan var2 var1 cs
        else bind' sSpan var1 var2 cs
  | (var1 `implements`) `all` traits2 = bind' sSpan var2 var1 cs
  | (var2 `implements`) `all` traits1 = bind' sSpan var1 var2 cs
  | otherwise = do
      newVar <- T.Var "x" (traits1 `Set.union` traits2) <$> lift varIdPlusPlus
      bind sSpan var1 newVar cs
        >>= either (return . Left) (bind sSpan var2 newVar)
        >>= either (return . Just) unify
unify ((CEqual sSpan var@(T.Var _ traits _) t) : cs) =
  if (t `implements`) `all` traits
    then bind' sSpan var t cs
    else return (Just (ExpectedTraits t traits sSpan))
unify ((CEqual sSpan pRec1@(T.PartialRecord fieldSpec1 _) pRec2@(T.PartialRecord fieldSpec2 _)) : cs) =
  let commonFieldCs = commonFieldConstraints sSpan fieldSpec1 fieldSpec2
      supersetFieldSpec = fieldUnion fieldSpec1 fieldSpec2
   in do
        supersetPartialRecord <- T.PartialRecord supersetFieldSpec <$> lift varIdPlusPlus
        bind sSpan pRec1 supersetPartialRecord cs
          >>= either (return . Left) (bind sSpan pRec2 supersetPartialRecord)
          >>= either (return . Just) (unify . (++ commonFieldCs))
  where
    -- Combine the field specs.  The new, "super" field spec will use the type
    -- in fs1, if it exists, or otherwise the field spec in fs2.
    fieldUnion :: T.FieldSpec -> T.FieldSpec -> T.FieldSpec
    fieldUnion = Map.unionWith const
unify ((CEqual sSpan (T.PartialRecord fieldSpec1 _) rec@(T.Record fieldSpec2)) : cs) =
  if fieldSpec1 `isFieldSubset` fieldSpec2
    then unify (commonFieldConstraints sSpan fieldSpec1 fieldSpec2 ++ cs)
    else return (Just (ExpectedFields rec fieldSpec1 sSpan))
  where
    isFieldSubset :: T.FieldSpec -> T.FieldSpec -> Bool
    isFieldSubset = List.isSubsequenceOf `on` (List.sort . Map.keys)
-- Swap
unify ((CEqual sSpan t var@(T.Var {})) : cs) = unify (CEqual sSpan var t : cs)
unify ((CEqual sSpan t pRec@(T.PartialRecord _ _)) : cs) = unify (CEqual sSpan pRec t : cs)
-- Conflict
unify ((CEqual sSpan t1 t2) : _) = return (Just (UnificationError t1 t2 sSpan))

commonFieldConstraints :: SAST.SourceSpan -> T.FieldSpec -> T.FieldSpec -> [Constraint]
commonFieldConstraints sSpan fieldSpec1 fieldSpec2 =
  Map.elems (Map.intersectionWith (CEqual sSpan) fieldSpec1 fieldSpec2)

implements :: T.Type -> T.Trait -> Bool
implements (T.Var _ varTraits _) trait = trait `elem` varTraits
implements T.Int T.Numeric             = True
implements T.Float T.Numeric           = True
implements _ _                         = False

bind ::
  SAST.SourceSpan ->
  T.Type ->
  T.Type ->
  [Constraint] ->
  Constrain (Either UnificationError [Constraint])
bind sSpan var t cs =
  if var `occursIn` t
    then return (Left (OccursError var t sSpan))
    else do
      modify (eliminateAndInsert var t)
      return (Right (map constraintEliminate cs))
  where
    subst = (var, t)
    constraintEliminate (CEqual sSpan' t1 t2) = CEqual sSpan' (applySubst subst t1) (applySubst subst t2)
    eliminateAndInsert :: T.Type -> T.Type -> Subst -> Subst
    eliminateAndInsert src target subs = Map.insert src target (Map.map (eliminate src target) subs)
    eliminate :: T.Type -> T.Type -> T.Type -> T.Type
    eliminate src target substType | substType == src = target
    eliminate src target (T.List substElemType) = T.List (eliminate src target substElemType)
    eliminate src target (T.Fun argTypes retType) = T.Fun (map (eliminate src target) argTypes) (eliminate src target retType)
    eliminate src target (T.Record fieldSpec) = T.Record (Map.map (eliminate src target) fieldSpec)
    eliminate _ _ substType = substType

bind' :: SAST.SourceSpan -> T.Type -> T.Type -> [Constraint] -> Constrain (Maybe UnificationError)
bind' sSpan var t cs = bind sSpan var t cs >>= either (return . Just) unify

occursIn :: T.Type -> T.Type -> Bool
occursIn _ T.Int = False
occursIn _ T.Float = False
occursIn _ T.Char = False
occursIn _ T.String = False
occursIn var (T.List elemType) = var `occursIn` elemType
occursIn var (T.Fun argTypes retType) = any (var `occursIn`) argTypes || var `occursIn` retType
occursIn var (T.Record fieldSpec) = any (var `occursIn`) fieldSpec
-- Algebraic types currently don't support vars (but will)
occursIn _ (T.AlgebraicType _) = False
occursIn var (T.TypeAlias _ t) = occursIn var t
occursIn var1 var2 | var1 == var2 = True
occursIn _ (T.Var {}) = False
occursIn var (T.PartialRecord fieldSpec _) = any (var `occursIn`) fieldSpec

applySubst :: (T.Type, T.Type) -> T.Type -> T.Type
applySubst _ T.Int = T.Int
applySubst _ T.Float = T.Float
applySubst _ T.Char = T.Char
applySubst _ T.String = T.String
applySubst subst (T.List elemType) = T.List (applySubst subst elemType)
applySubst subst (T.Fun argTypes retType) = T.Fun (map (applySubst subst) argTypes) (applySubst subst retType)
applySubst subst (T.Record fieldSpec) = T.Record (Map.map (applySubst subst) fieldSpec)
applySubst _ (T.AlgebraicType name) = T.AlgebraicType name
applySubst subst (T.TypeAlias name t) = T.TypeAlias name (applySubst subst t)
applySubst (var1, rep) var2 | var1 == var2 = rep
applySubst _ var@(T.Var {}) = var
applySubst subst (T.PartialRecord fieldSpec prId) = T.PartialRecord (Map.map (applySubst subst) fieldSpec) prId

valueType ::
  QualifiedReference -> ExceptT [UnificationError] (State CompilerState) [TAST.TypedValueDef]
valueType qvr = do
    unificationRes <- valueType' qvr
    case unificationRes of
      Right tvds -> mapM_ setCache tvds $> Right (List.nubBy ((==) `on` fst) tvds)
      Left errs -> return (Left errs)
  where
    valueType' ::
      QualifiedReference -> State CompilerState (Either [UnificationError] [TAST.TypedValueDef])
    valueType' qvr' = do
      constraintsRes <- C.valueConstraints qvr'
      case constraintsRes of
        Right
          (C.ConstrainRes {_typedExpr = expr, _constraints = constraints, _pendingTypes' = pendingTypeRefs}) -> do
          -- Populate the cache with an unrefined value to prevent circular references from creating infinite loops
          setCache (qvr', expr)
          -- Create additional constraints by finding the types of dependent values
          depRes <- foldM foldDep (Right ([], [])) pendingTypeRefs
          case depRes of
            (Left errs) -> return (Left errs)
            (Right (depConstraints, tvds)) -> do
              res <- runStateT (unify (constraints ++ depConstraints)) Map.empty
              case res of
                (Just err, _) -> return (Left [err])
                (Nothing, subst) -> return (Right ((qvr', resolveTypeAnno subst expr) : tvds)) -- TODO: "populate down"
        Left constrainErrs -> return (Left (map ConstraintError constrainErrs))
    foldDep ::
      Either [UnificationError] ([Constraint], [TAST.TypedValueDef]) ->
      (QualifiedReference, T.Type, SAST.SourceSpan) ->
      State CompilerState (Either [UnificationError] ([Constraint], [TAST.TypedValueDef]))
    foldDep (Right (cs, tvds)) (qvr'', t, s) = do
      typedExprRes <- valueType qvr''
      case typedExprRes of
        Right typedExprs -> do
          dupedDepType <- dupTypeVars (TAST.typeOf (snd (head typedExprs)))
          return (Right (CEqual s t dupedDepType : cs, typedExprs ++ tvds))
        Left errs -> return (Left errs)
    foldDep (Left errs) _ = return (Left errs)
    resolveTypeAnno :: Subst -> TAST.Expr -> TAST.Expr
    resolveTypeAnno subst (TAST.EVar name sst) = TAST.EVar name (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.EConstructor name sst) = TAST.EConstructor name (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.ELit lit sst) = TAST.ELit lit (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.ELambda params body sst) = TAST.ELambda params (resolveTypeAnno subst body) (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.EApp fun args sst) =
      TAST.EApp (resolveTypeAnno subst fun) (map (resolveTypeAnno subst) args) (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.ECase subj branches sst) =
      TAST.ECase
        (resolveTypeAnno subst subj)
        (map (bimap (resolvePatternTypeAnno' subst) (resolveTypeAnno subst)) branches)
        (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.EBinary op left right sst) =
      TAST.EBinary
        op
        (resolveTypeAnno subst left)
        (resolveTypeAnno subst right)
        (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.ERecord fieldSpec sst) = TAST.ERecord (map (second (resolveTypeAnno subst)) fieldSpec) (maybeSubType subst sst)
    resolveTypeAnno subst (TAST.EList elems sst) = TAST.EList (map (resolveTypeAnno subst) elems) (maybeSubType subst sst)
    resolvePatternTypeAnno' :: Subst -> TAST.Pattern -> TAST.Pattern
    resolvePatternTypeAnno' subst (TAST.PVar name sst) = TAST.PVar name (maybeSubType subst sst)
    resolvePatternTypeAnno' subst (TAST.PDiscard sst) = TAST.PDiscard (maybeSubType subst sst)
    resolvePatternTypeAnno' subst (TAST.PConstructor name params sst) = TAST.PConstructor name (map (resolvePatternTypeAnno' subst) params) (maybeSubType subst sst)
    resolvePatternTypeAnno' subst (TAST.PRecord fieldSpec sst) = TAST.PRecord (map (second (resolvePatternTypeAnno' subst)) fieldSpec) (maybeSubType subst sst)
    resolvePatternTypeAnno' subst (TAST.PList elems sst) = TAST.PList (map (resolvePatternTypeAnno' subst) elems) (maybeSubType subst sst)
    resolvePatternTypeAnno' subst (TAST.PLiteral lit sst) = TAST.PLiteral lit (maybeSubType subst sst)
    maybeSubType :: Subst -> TAST.SourceSpanType -> TAST.SourceSpanType
    maybeSubType subst sst = sst {TAST._type = foldl (flip applySubst) (TAST._type sst) (Map.toList subst)}
    dupTypeVars :: T.Type -> State CompilerState T.Type
    dupTypeVars t = evalStateT (dupTypeVars' t) Map.empty
    dupTypeVars' :: T.Type -> Constrain T.Type
    dupTypeVars' T.Int = return T.Int
    dupTypeVars' T.Float = return T.Float
    dupTypeVars' T.Char = return T.Char
    dupTypeVars' T.String = return T.String
    dupTypeVars' (T.List var) = T.List <$> dupTypeVars' var
    dupTypeVars' (T.Fun argTypes retType) = T.Fun <$> mapM dupTypeVars' argTypes <*> dupTypeVars' retType
    dupTypeVars' (T.Record fieldSpec) =
      T.Record . Map.fromList <$> mapM (\(name, t) -> (name,) <$> dupTypeVars' t) (Map.toList fieldSpec)
    dupTypeVars' (T.AlgebraicType name) = return (T.AlgebraicType name)
    dupTypeVars' (T.TypeAlias name t) = T.TypeAlias name <$> dupTypeVars' t
    dupTypeVars' var@(T.Var name traits _) = do
      existingTypeRes <- gets (!? var)
      case existingTypeRes of
        Just t -> return t
        Nothing -> do
          newVar <- T.Var name traits <$> lift varIdPlusPlus
          modify (Map.insert var newVar)
          return newVar
    dupTypeVars' recVar@(T.PartialRecord fieldSpec _) = do
      existingTypeRes <- gets (!? recVar)
      case existingTypeRes of
        Just t -> return t
        Nothing -> do
          newRecVar <-
            T.PartialRecord . Map.fromList
              <$> mapM (\(name, t) -> (name,) <$> dupTypeVars' t) (Map.toList fieldSpec)
              <*> lift varIdPlusPlus
          modify (Map.insert recVar newRecVar)
          return newRecVar
