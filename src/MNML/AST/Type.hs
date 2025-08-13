module MNML.AST.Type
    ( Annotated (..)
    , Constructor (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpanType (..)
    , TypeDef (..)
    , Typed (..)
    , ValueDef (..)
    , nodeSpan
    , nodeType
    , setNodeType
    ) where

import           Data.Bifunctor (bimap, second)
import           Data.Text      (Text)
import           MNML.AST.Span  (SourceSpan (..), Spanned (..))
import qualified MNML.Type      as T
import           Text.Parsec    (SourcePos)

data TypeDef
  = TypeDef T.Type SourceSpan
  deriving (Eq, Show)

-- The span refers to the entire span of the definition whereas the Expr's span
-- refers only to the RHS
data ValueDef
  = ValueDef (Expr SourceSpanType) SourceSpan
  deriving (Eq, Show)

-- e.g. Just Int; Name, TypeArgs, Span
data Constructor
  = Constructor Text [(T.Type, SourceSpan)] SourceSpan
  deriving (Eq, Show)

data SourceSpanType
  = SourceSpanType
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      , _type      :: T.Type
      }
  deriving (Eq, Show)

data Expr anno
  = EVar Text anno
  | EConstructor Text anno -- Foo
  | ELit (Literal anno) anno
  | ELambda [Text] (Expr anno) anno -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp (Expr anno) [Expr anno] anno -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase (Expr anno) [(Pattern anno, Expr anno)] anno
  | EBinary Operator (Expr anno) (Expr anno) anno
  | ERecord [(Text, Expr anno)] anno
  | EList [Expr anno] anno
  deriving (Eq, Show)

data Literal anno
  = LInt Integer anno
  | LFloat Double anno
  | LChar Char anno
  | LString Text anno
  deriving (Eq, Show)

data Pattern anno
  = PVar Text anno
  | PDiscard anno -- _
  | PConstructor Text [Pattern anno] anno
  | PRecord [(Text, Pattern anno)] anno
  | PList [Pattern anno] anno
  | PLiteral (Literal anno) anno
  deriving (Eq, Show)

data Operator = Add | Sub | Mul | Div | And | Or | Equals
  deriving (Eq, Show)

class Typed a where
  typeOf :: a -> T.Type
  setType :: a -> T.Type -> a

nodeType :: (Annotated node, Typed anno) => node anno -> T.Type
nodeType = typeOf . getAnno

setNodeType :: (Annotated node, Typed anno) => node anno -> T.Type -> node anno
setNodeType node t = setAnno node (setType (getAnno node) t)

nodeSpan :: (Annotated node, Spanned anno) => node anno -> SourceSpan
nodeSpan = spanOf . getAnno

class Annotated a where
  getAnno :: a b -> b
  setAnno :: a b -> b -> a b

instance Typed SourceSpanType where
  typeOf = _type
  setType spanA t = spanA {_type = t}

instance Spanned SourceSpanType where
  spanOf (SourceSpanType {_spanStart = start, _spanEnd = end}) = (SourceSpan {_spanStart = start, _spanEnd = end})

instance Functor Expr where
  fmap f (EVar name spanA)           = EVar name (f spanA)
  fmap f (EConstructor name spanA)   = EConstructor name (f spanA)
  fmap f (ELit lit spanA)            = ELit (fmap f lit) (f spanA)
  fmap f (ELambda args body spanA)   = ELambda args (fmap f body) (f spanA)
  fmap f (EApp applicant args spanA) = EApp (fmap f applicant) (map (fmap f) args) (f spanA)
  fmap f (ECase subject arms spanA)  = ECase (fmap f subject) (map (bimap (fmap f) (fmap f)) arms) (f spanA)
  fmap f (EBinary op lhs rhs spanA)  = EBinary op (fmap f lhs) (fmap f rhs) (f spanA)
  fmap f (ERecord fieldSpec spanA)   = ERecord (map (second (fmap f)) fieldSpec) (f spanA)
  fmap f (EList members spanA)       = EList (map (fmap f) members) (f spanA)

instance Functor Literal where
  fmap f (LInt i spanA)      = LInt i (f spanA)
  fmap f (LFloat float anno) = LFloat float (f anno)
  fmap f (LChar c anno)      = LChar c (f anno)
  fmap f (LString s anno)    = LString s (f anno)

instance Functor Pattern where
  fmap f (PVar name anno)                 = PVar name (f anno)
  fmap f (PDiscard anno)                  = PDiscard (f anno)
  fmap f (PConstructor name argPats anno) = PConstructor name (map (fmap f) argPats) (f anno)
  fmap f (PRecord fieldSpec anno)         = PRecord (map (second (fmap f)) fieldSpec) (f anno)
  fmap f (PList members anno)             = PList (map (fmap f) members) (f anno)
  fmap f (PLiteral lit anno)              = PLiteral (fmap f lit) (f anno)

instance Annotated Expr where
  getAnno (EVar _ anno)         = anno
  getAnno (EConstructor _ anno) = anno
  getAnno (ELit _ anno)         = anno
  getAnno (ELambda _ _ anno)    = anno
  getAnno (EApp _ _ anno)       = anno
  getAnno (ECase _ _ anno)      = anno
  getAnno (EBinary _ _ _ anno)  = anno
  getAnno (ERecord _ anno)      = anno
  getAnno (EList _ anno)        = anno

  setAnno (EVar name _) anno           = EVar name anno
  setAnno (EConstructor name _) anno   = EConstructor name anno
  setAnno (ELit lit _) anno            = ELit lit anno
  setAnno (ELambda args body _) anno   = ELambda args body anno
  setAnno (EApp applicant args _) anno = EApp applicant args anno
  setAnno (ECase subject arms _) anno  = ECase subject arms anno
  setAnno (EBinary op lhs rhs _) anno  = EBinary op lhs rhs anno
  setAnno (ERecord fieldSpec _) anno   = ERecord fieldSpec anno
  setAnno (EList members _) anno       = EList members anno

instance Annotated Literal where
  getAnno (LInt _ anno)    = anno
  getAnno (LFloat _ anno)  = anno
  getAnno (LChar _ anno)   = anno
  getAnno (LString _ anno) = anno

  setAnno (LInt i _) anno    = LInt i anno
  setAnno (LFloat f _) anno  = LFloat f anno
  setAnno (LChar c _) anno   = LChar c anno
  setAnno (LString s _) anno = LString s anno

instance Annotated Pattern where
  getAnno (PVar _ anno)           = anno
  getAnno (PDiscard anno)         = anno
  getAnno (PConstructor _ _ anno) = anno
  getAnno (PRecord _ anno)        = anno
  getAnno (PList _ anno)          = anno
  getAnno (PLiteral _ anno)       = anno

  setAnno (PVar name _) anno              = PVar name anno
  setAnno (PDiscard _) anno               = PDiscard anno
  setAnno (PConstructor name args _) anno = PConstructor name args anno
  setAnno (PRecord fieldSpec _) anno      = PRecord fieldSpec anno
  setAnno (PList members _) anno          = PList members anno
  setAnno (PLiteral lit _) anno           = PLiteral lit anno
