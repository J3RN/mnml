module MNML.Unify
    ( UnificationError (..)
    , valueType
    ) where

import           Control.Monad.State (State, modify, runState)
import           Data.Bifunctor      (second)
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           MNML                (CompilerState (..))
import qualified MNML.AST            as AST
import qualified MNML.Constrain      as C
import qualified MNML.Parse          as P
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
  | ConstraintError C.ConstraintError
  | ParseError P.ParseError
  deriving (Eq, Show)

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
      fieldConstraints = map (\(_, (t1, t2)) -> T.CEqual t1 t2 nodeId) commonFields
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
implements T.Int T.Numeric              = True
implements T.Float T.Numeric            = True
implements _ _                          = False

intersectWith :: (Ord a) => (b -> b -> c) -> [(a, b)] -> [(a, b)] -> [(a, c)]
intersectWith comb a b = Map.toList ((Map.intersectionWith comb `on` Map.fromList) a b)

bind :: AST.NodeId -> T.Type -> T.Type -> [T.Constraint] -> State Subst (Maybe UnificationError)
bind nodeId var t cs =
  if var `occursIn` t
    then return (Just (OccursError var t nodeId))
    else do
      modify (eliminateAndInsert var t)
      unify
        ( map
            ( \case
                (T.CEqual t1 t2 nodeId') -> T.CEqual (applySubst subst t1) (applySubst subst t2) nodeId'
            )
            cs
        )
  where
    subst = (var, t)
    eliminateAndInsert :: T.Type -> T.Type -> Subst -> Subst
    eliminateAndInsert src target = Map.insert src target . Map.map (eliminate src target)
    eliminate :: T.Type -> T.Type -> T.Type -> T.Type
    eliminate src target substType | substType == src = target
    eliminate src target (T.List substElemType) = T.List (eliminate src target substElemType)
    eliminate src target (T.Fun argTypes retType) = T.Fun (map (eliminate src target) argTypes) (eliminate src target retType)
    eliminate src target (T.Record fieldSpec) = T.Record (map (second (eliminate src target)) fieldSpec)
    eliminate _ _ substType = substType

occursIn :: T.Type -> T.Type -> Bool
-- Type aliases currently have to be qualified, so no vars
occursIn var (T.TypeAlias _ t) = occursIn var t
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
applySubst _ T.Int = T.Int
applySubst _ T.Float = T.Float
applySubst _ T.Char = T.Char
applySubst _ T.String = T.String
applySubst subst (T.List elemType) = T.List (applySubst subst elemType)
applySubst subst (T.Fun argTypes retType) = T.Fun (map (applySubst subst) argTypes) (applySubst subst retType)
applySubst subst (T.Record fieldSpec) = T.Record (map (second (applySubst subst)) fieldSpec)
applySubst _ (T.AlgebraicType name) = T.AlgebraicType name
applySubst subst (T.TypeAlias name t) = T.TypeAlias name (applySubst subst t)
applySubst (var1, rep) var2 | var1 == var2 = rep
applySubst _ var@(T.Var _ _ _) = var

valueType :: Text -> Text -> State CompilerState (Either UnificationError T.Type)
valueType modu valName = do
  constraintsRes <- C.valueConstraints modu valName
  case constraintsRes of
    Right (inferType, constraints) ->
      case runState (unify constraints) Map.empty of
        (Just err, _) -> return (Left err)
        (Nothing, subst) -> return (Right (fromMaybe inferType (subst !? inferType)))
    Left constrainErr -> return (Left (ConstraintError constrainErr))
