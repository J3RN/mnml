module MNML.AST
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpan (..)
    , SpanAnnotation (..)
    , Type (..)
    , getAnno
    ) where

import           Data.Bifunctor (bimap, second)
import           Data.Text      (Text)
import           Text.Parsec    (SourcePos)

data SourceSpan
  = SourceSpan
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      }
  deriving (Eq, Show)

newtype SpanAnnotation
  = SpanAnnotation SourceSpan
  deriving (Eq, Show)

data Declaration a
  = TypeDecl Text [(Text, [Type a])] a -- "MyType = Foo(String) | Bar(Int, String)"
  | TypeAliasDecl Text (Type a) a -- "alias  {name: String} as User" or "alias Int as Price"
  | ValueDecl Text (Expr a) a
  deriving (Eq, Show)

data Expr a
  = EVar Text a
  | EConstructor Text a -- Foo
  | ELit (Literal a) a
  | ELambda [Text] (Expr a) a -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp (Expr a) [Expr a] a -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase (Expr a) [(Pattern a, Expr a)] a
  | EBinary Operator (Expr a) (Expr a) a
  | ERecord [(Text, Expr a)] a
  | EList [Expr a] a
  deriving (Eq, Show)

data Literal a
  = LInt Integer a
  | LFloat Double a
  | LChar Char a
  | LString Text a
  deriving (Eq, Show)

data Pattern a
  = PVar Text a
  | PDiscard a -- _
  | PConstructor Text [Pattern a] a
  | PRecord [(Text, Pattern a)] a
  | PList [Pattern a] a
  | PLiteral (Literal a) a
  deriving (Eq, Show)

data Operator = Add | Sub | Mul | Div | And | Or | Equals
  deriving (Eq, Show)

data Type a
  = TInt a
  | TFloat a
  | TChar a
  | TString a
  | TNamedType Text a -- "User"
  | TList (Type a) a -- TInt
  | TFun [Type a] (Type a) a -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type a)] a -- [("name", TString), ...]
  | TVar Text a -- "a"
  deriving (Eq, Show)

instance Functor Declaration where
  fmap f (TypeDecl name cons a) = TypeDecl name (map (second (map (fmap f))) cons) (f a)
  fmap f (TypeAliasDecl name t  a) = TypeAliasDecl name (fmap f t) (f a)
  fmap f (ValueDecl name expr a) = ValueDecl name (fmap f expr) (f a)

instance Functor Expr where
  fmap f (EVar name a)          = EVar name (f a)
  fmap f (EConstructor name a)  = EConstructor name (f a)
  fmap f (ELit lit a)           = ELit (fmap f lit) (f a)
  fmap f (ELambda args body a)  = ELambda args (fmap f body) (f a)
  fmap f (EApp fun args a)      = EApp (fmap f fun) (map (fmap f) args) (f a)
  fmap f (ECase sub branches a) = ECase (fmap f sub) (map (bimap (fmap f) (fmap f)) branches) (f a)
  fmap f (EBinary op e1 e2 a)   = EBinary op (fmap f e1) (fmap f e2) (f a)
  fmap f (ERecord fieldSpec a)  = ERecord (map (second (fmap f)) fieldSpec) (f a)
  fmap f (EList elems a)        = EList (map (fmap f) elems) (f a)

getAnno :: Expr a -> a
getAnno (EVar _ a)         = a
getAnno (EConstructor _ a) = a
getAnno (ELit _ a)         = a
getAnno (ELambda _ _ a)    = a
getAnno (EApp _ _ a)       = a
getAnno (ECase _ _ a)      = a
getAnno (EBinary _ _ _ a)  = a
getAnno (ERecord _ a)      = a
getAnno (EList _ a)        = a

instance Functor Literal where
  fmap f (LInt i a)    = LInt i (f a)
  fmap f (LFloat fl a) = LFloat fl (f a)
  fmap f (LChar c a)   = LChar c (f a)
  fmap f (LString s a) = LString s (f a)

instance Functor Pattern where
  fmap f (PVar v a)                 = PVar v (f a)
  fmap f (PDiscard a)               = PDiscard (f a)
  fmap f (PConstructor name cons a) = PConstructor name (map (fmap f) cons) (f a)
  fmap f (PRecord fieldSpec a)      = PRecord (map (second (fmap f)) fieldSpec) (f a)
  fmap f (PList elems a)            = PList (map (fmap f) elems) (f a)
  fmap f (PLiteral lit a)           = PLiteral (fmap f lit) (f a)

instance Functor Type where
  fmap f (TInt a)              = TInt (f a)
  fmap f (TFloat  a)           = TFloat  (f a)
  fmap f (TChar  a)            = TChar  (f a)
  fmap f (TString a)           = TString (f a)
  fmap f (TNamedType name a)   = TNamedType name (f a)
  fmap f (TList elemT a)       = TList (fmap f elemT)  (f a)
  fmap f (TFun argTs retT a)   = TFun (map (fmap f) argTs) (fmap f retT) (f a)
  fmap f (TRecord fieldSpec a) = TRecord (map (second (fmap f)) fieldSpec) (f a)
  fmap f (TVar name a)         = TVar name (f a)
