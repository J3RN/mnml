module MNML.Unify
    ( unify
    ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (State, StateT, execStateT, lift, modify,
                                       runStateT)
import           Data.Bifunctor       (bimap, second)
import           Data.Function        (on)
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Lens.Micro           (Lens', lens, over)
import qualified MNML.AST.Span        as SAST
import qualified MNML.AST.Type        as TAST
import           MNML.CompilerState   (CompilerState (..), varIdPlusPlus)
import           MNML.Constraint      (Constraint (..))
import           MNML.Error           (Error (UnificationError), Fallible,
                                       UnificationError (..))
import qualified MNML.Type            as T

-- Some helpful aliases
type Expr = TAST.Expr TAST.SourceSpanType
type Pattern = TAST.Pattern TAST.SourceSpanType
type Literal = TAST.Literal TAST.SourceSpanType

-- Really, Subst is a mapping of type *variables* to types (variables or otherwise)
type Subst = Map T.Type T.Type

data UnifyEnv
  = UnifyEnv
      { _subst  :: Subst
      , _errors :: [UnificationError]
      }

subst :: Lens' UnifyEnv Subst
subst = lens _subst (\ue subs -> ue { _subst = subs })

errors :: Lens' UnifyEnv [UnificationError]
errors = lens _errors (\ue errs -> ue { _errors = errs })

type Unify = StateT UnifyEnv (State CompilerState)

addError :: UnificationError -> Unify ()
addError err = modify (over errors (err:))

-- Unify a set of constraints

unify' :: [Constraint] -> Unify (Maybe UnificationError)
unify' [] = return Nothing
-- Delete
unify' ((CEqual _ t1 t2) : cs) | t1 == t2 = unify' cs
-- Decompose
unify' ((CEqual sSpan (T.List a) (T.List b)) : cs) = unify' (CEqual sSpan a b : cs)
unify' ((CEqual sSpan (T.Fun argTypes1 retType1) (T.Fun argTypes2 retType2)) : cs) =
  if length argTypes1 /= length argTypes2
    then return (Just (ArgumentLengthMismatch sSpan))
    else
      let retCon = CEqual sSpan retType1 retType2
          argTypeCons = zipWith (CEqual sSpan) argTypes1 argTypes2
       in unify' (retCon : (argTypeCons ++ cs))
unify' ((CEqual sSpan rec1@(T.Record fieldSpec1) rec2@(T.Record fieldSpec2)) : cs) =
  if List.sort (Map.keys fieldSpec1) == List.sort (Map.keys fieldSpec2)
    then unify' (commonFieldConstraints sSpan fieldSpec1 fieldSpec2 ++ cs)
    else return (Just (UnifyError rec1 rec2 sSpan))
-- Eliminate
unify' ((CEqual sSpan var1@(T.Var _ traits1 id1) var2@(T.Var _ traits2 id2)) : cs)
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
        >>= either (return . Just) unify'
unify' ((CEqual sSpan var@(T.Var _ traits _) t) : cs) =
  if (t `implements`) `all` traits
    then bind' sSpan var t cs
    else return (Just (ExpectedTraits t traits sSpan))
unify' ((CEqual sSpan pRec1@(T.PartialRecord fieldSpec1 _) pRec2@(T.PartialRecord fieldSpec2 _)) : cs) =
  let commonFieldCs = commonFieldConstraints sSpan fieldSpec1 fieldSpec2
      supersetFieldSpec = fieldUnion fieldSpec1 fieldSpec2
   in do
        supersetPartialRecord <- T.PartialRecord supersetFieldSpec <$> lift varIdPlusPlus
        bind sSpan pRec1 supersetPartialRecord cs
          >>= either (return . Left) (bind sSpan pRec2 supersetPartialRecord)
          >>= either (return . Just) (unify' . (++ commonFieldCs))
  where
    -- Combine the field specs.  The new, "super" field spec will use the type
    -- in fs1, if it exists, or otherwise the field spec in fs2.
    fieldUnion :: T.FieldSpec -> T.FieldSpec -> T.FieldSpec
    fieldUnion = Map.unionWith const
unify' ((CEqual sSpan (T.PartialRecord fieldSpec1 _) rec@(T.Record fieldSpec2)) : cs) =
  if fieldSpec1 `isFieldSubset` fieldSpec2
    then unify' (commonFieldConstraints sSpan fieldSpec1 fieldSpec2 ++ cs)
    else return (Just (ExpectedFields rec fieldSpec1 sSpan))
  where
    isFieldSubset :: T.FieldSpec -> T.FieldSpec -> Bool
    isFieldSubset = List.isSubsequenceOf `on` (List.sort . Map.keys)
-- Swap
unify' ((CEqual sSpan t var@(T.Var {})) : cs) = unify' (CEqual sSpan var t : cs)
unify' ((CEqual sSpan t pRec@(T.PartialRecord _ _)) : cs) = unify' (CEqual sSpan pRec t : cs)
-- Conflict
unify' ((CEqual sSpan t1 t2) : _) = return (Just (UnifyError t1 t2 sSpan))

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
  Unify (Either UnificationError [Constraint])
bind sSpan var t cs =
  if var `occursIn` t
    then return (Left (OccursError var t sSpan))
    else do
      modify (over subst (eliminateAndInsert var t))
      return (Right (map constraintEliminate cs))
  where
    subst' = (var, t)
    constraintEliminate (CEqual sSpan' t1 t2) = CEqual sSpan' (applySubst subst' t1) (applySubst subst' t2)
    eliminateAndInsert :: T.Type -> T.Type -> Subst -> Subst
    eliminateAndInsert src target subs = Map.insert src target (Map.map (eliminate src target) subs)
    eliminate :: T.Type -> T.Type -> T.Type -> T.Type
    eliminate src target substType | substType == src = target
    eliminate src target (T.List substElemType) = T.List (eliminate src target substElemType)
    eliminate src target (T.Fun argTypes retType) = T.Fun (map (eliminate src target) argTypes) (eliminate src target retType)
    eliminate src target (T.Record fieldSpec) = T.Record (Map.map (eliminate src target) fieldSpec)
    eliminate _ _ substType = substType

bind' :: SAST.SourceSpan -> T.Type -> T.Type -> [Constraint] -> Unify (Maybe UnificationError)
bind' sSpan var t cs = bind sSpan var t cs >>= either (return . Just) unify'

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
applySubst subs (T.List elemType) = T.List (applySubst subs elemType)
applySubst subs (T.Fun argTypes retType) = T.Fun (map (applySubst subs) argTypes) (applySubst subs retType)
applySubst subs (T.Record fieldSpec) = T.Record (Map.map (applySubst subs) fieldSpec)
applySubst _ (T.AlgebraicType name) = T.AlgebraicType name
applySubst subs (T.TypeAlias name t) = T.TypeAlias name (applySubst subs t)
applySubst (var1, rep) var2 | var1 == var2 = rep
applySubst _ var@(T.Var {}) = var
applySubst subs (T.PartialRecord fieldSpec prId) = T.PartialRecord (Map.map (applySubst subs) fieldSpec) prId

unify :: (TAST.Batch, [Constraint]) -> Fallible TAST.Batch
unify (batch, cs) = do
  -- TODO: Really unify' shouldn't return anything; the errors should be in the env
  (maybeErr, env) <- lift (runStateT (unify' cs) (UnifyEnv {_errors = [], _subst = Map.empty}))
  case maybeErr of
    Just err -> throwError [UnificationError err]
    Nothing  -> return (batch {TAST._valueDefs = Map.map (resolveTypeAnno (_subst env)) (TAST._valueDefs batch)})

resolveTypeAnno :: Subst -> TAST.ValueDef -> TAST.ValueDef
resolveTypeAnno subs (TAST.ValueDef expr spanA) = TAST.ValueDef (resolveTypeAnno' subs expr) spanA

resolveTypeAnno' :: Subst -> Expr -> Expr
resolveTypeAnno' subs (TAST.EVar name sst) = TAST.EVar name (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.EConstructor name sst) = TAST.EConstructor name (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.ELit lit sst) = TAST.ELit lit (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.ELambda params body sst) = TAST.ELambda params (resolveTypeAnno' subs body) (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.EApp fun args sst) =
  TAST.EApp (resolveTypeAnno' subs fun) (map (resolveTypeAnno' subs) args) (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.ECase subj branches sst) =
  TAST.ECase
    (resolveTypeAnno' subs subj)
    (map (bimap (resolvePatternTypeAnno subs) (resolveTypeAnno' subs)) branches)
    (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.EBinary op left right sst) =
  TAST.EBinary
    op
    (resolveTypeAnno' subs left)
    (resolveTypeAnno' subs right)
    (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.ERecord fieldSpec sst) = TAST.ERecord (map (second (resolveTypeAnno' subs)) fieldSpec) (maybeSubType subs sst)
resolveTypeAnno' subs (TAST.EList elems sst) = TAST.EList (map (resolveTypeAnno' subs) elems) (maybeSubType subs sst)

resolvePatternTypeAnno :: Subst -> Pattern -> Pattern
resolvePatternTypeAnno subs (TAST.PVar name sst) = TAST.PVar name (maybeSubType subs sst)
resolvePatternTypeAnno subs (TAST.PDiscard sst) = TAST.PDiscard (maybeSubType subs sst)
resolvePatternTypeAnno subs (TAST.PConstructor name params sst) = TAST.PConstructor name (map (resolvePatternTypeAnno subs) params) (maybeSubType subs sst)
resolvePatternTypeAnno subs (TAST.PRecord fieldSpec sst) = TAST.PRecord (map (second (resolvePatternTypeAnno subs)) fieldSpec) (maybeSubType subs sst)
resolvePatternTypeAnno subs (TAST.PList elems sst) = TAST.PList (map (resolvePatternTypeAnno subs) elems) (maybeSubType subs sst)
resolvePatternTypeAnno subs (TAST.PLiteral lit sst) = TAST.PLiteral lit (maybeSubType subs sst)

maybeSubType :: Subst -> TAST.SourceSpanType -> TAST.SourceSpanType
maybeSubType subs sst = sst {TAST._type = foldl (flip applySubst) (TAST._type sst) (Map.toList subs)}
