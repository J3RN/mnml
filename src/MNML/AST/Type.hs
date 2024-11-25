module MNML.AST.Type
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpanType (..)
    , Type (..)
    , Typed (..)
    ) where

import           Data.Text     (Text)
import           MNML.AST.Span (Spanned (..))
import qualified MNML.Type     as T
import           Text.Parsec   (SourcePos)

data SourceSpanType
  = SourceSpanType
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      , _type      :: T.Type
      }
  deriving (Eq, Show)

data Declaration
  = TypeDecl Text [(Text, [Type])] SourceSpanType -- "MyType = Foo(String) | Bar(Int, String)"
  | TypeAliasDecl Text Type SourceSpanType -- "alias  {name: String} as User" or "alias Int as Price"
  | ValueDecl Text Expr SourceSpanType
  deriving (Eq, Show)

data Expr
  = EVar Text SourceSpanType
  | EConstructor Text SourceSpanType -- Foo
  | ELit Literal SourceSpanType
  | ELambda [Text] Expr SourceSpanType -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] SourceSpanType -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)] SourceSpanType
  | EBinary Operator Expr Expr SourceSpanType
  | ERecord [(Text, Expr)] SourceSpanType
  | EList [Expr] SourceSpanType
  deriving (Eq, Show)

data Literal
  = LInt Integer SourceSpanType
  | LFloat Double SourceSpanType
  | LChar Char SourceSpanType
  | LString Text SourceSpanType
  deriving (Eq, Show)

data Pattern
  = PVar Text SourceSpanType
  | PDiscard SourceSpanType -- _
  | PConstructor Text [Pattern] SourceSpanType
  | PRecord [(Text, Pattern)] SourceSpanType
  | PList [Pattern] SourceSpanType
  | PLiteral Literal SourceSpanType
  deriving (Eq, Show)

data Operator = Add | Sub | Mul | Div | And | Or | Equals
  deriving (Eq, Show)

data Type
  = TInt SourceSpanType
  | TFloat SourceSpanType
  | TChar SourceSpanType
  | TString SourceSpanType
  | TNamedType Text SourceSpanType -- "User"
  | TList Type SourceSpanType -- TInt
  | TFun [Type] Type SourceSpanType -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type)] SourceSpanType -- [("name", TString), ...]
  | TVar Text SourceSpanType -- "a"
  deriving (Eq, Show)

class Typed a where
  typeOf :: a -> T.Type

instance Typed Declaration where
  typeOf (TypeDecl _ _ (SourceSpanType {_type = t}))      = t
  typeOf (TypeAliasDecl _ _ (SourceSpanType {_type = t})) = t
  typeOf (ValueDecl _ _ (SourceSpanType {_type = t}))     = t

instance Spanned Declaration where
  spanOf (TypeDecl _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (TypeAliasDecl _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ValueDecl _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)

instance Typed Expr where
  typeOf (EVar _ (SourceSpanType {_type = t}))         = t
  typeOf (EConstructor _ (SourceSpanType {_type = t})) = t
  typeOf (ELit _ (SourceSpanType {_type = t}))         = t
  typeOf (ELambda _ _ (SourceSpanType {_type = t}))    = t
  typeOf (EApp _ _ (SourceSpanType {_type = t}))       = t
  typeOf (ECase _ _ (SourceSpanType {_type = t}))      = t
  typeOf (EBinary _ _ _ (SourceSpanType {_type = t}))  = t
  typeOf (ERecord _ (SourceSpanType {_type = t}))      = t
  typeOf (EList _ (SourceSpanType {_type = t}))        = t

instance Spanned Expr where
  spanOf (EVar _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (EConstructor _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ELit _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ELambda _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (EApp _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ECase _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (EBinary _ _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ERecord _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (EList _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)

instance Typed Literal where
  typeOf (LInt _ (SourceSpanType {_type = t}))    = t
  typeOf (LFloat _ (SourceSpanType {_type = t}))  = t
  typeOf (LChar _ (SourceSpanType {_type = t}))   = t
  typeOf (LString _ (SourceSpanType {_type = t})) = t

instance Spanned Literal where
  spanOf (LInt _ (SourceSpanType {_spanStart = s, _spanEnd = e}))    = (s, e)
  spanOf (LFloat _ (SourceSpanType {_spanStart = s, _spanEnd = e}))  = (s, e)
  spanOf (LChar _ (SourceSpanType {_spanStart = s, _spanEnd = e}))   = (s, e)
  spanOf (LString _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)

instance Typed Pattern where
  typeOf (PVar _ (SourceSpanType {_type = t}))           = t
  typeOf (PDiscard (SourceSpanType {_type = t}))         = t
  typeOf (PConstructor _ _ (SourceSpanType {_type = t})) = t
  typeOf (PRecord _ (SourceSpanType {_type = t}))        = t
  typeOf (PList _ (SourceSpanType {_type = t}))          = t
  typeOf (PLiteral _ (SourceSpanType {_type = t}))       = t

instance Spanned Pattern where
  spanOf (PVar _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PDiscard (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PConstructor _ _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PRecord _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PList _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PLiteral _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)

-- Haha
instance Typed Type where
  typeOf (TInt (SourceSpanType {_type = t}))         = t
  typeOf (TFloat (SourceSpanType {_type = t}))       = t
  typeOf (TChar (SourceSpanType {_type = t}))        = t
  typeOf (TString (SourceSpanType {_type = t}))      = t
  typeOf (TNamedType _ (SourceSpanType {_type = t})) = t
  typeOf (TList _ (SourceSpanType {_type = t}))      = t
  typeOf (TFun _ _ (SourceSpanType {_type = t}))     = t
  typeOf (TRecord _ (SourceSpanType {_type = t}))    = t
  typeOf (TVar _ (SourceSpanType {_type = t}))       = t

instance Spanned Type where
  spanOf (TInt (SourceSpanType {_spanStart = s, _spanEnd = e}))         = (s, e)
  spanOf (TFloat (SourceSpanType {_spanStart = s, _spanEnd = e}))       = (s, e)
  spanOf (TChar (SourceSpanType {_spanStart = s, _spanEnd = e}))        = (s, e)
  spanOf (TString (SourceSpanType {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (TNamedType _ (SourceSpanType {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (TList _ (SourceSpanType {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (TFun _ _ (SourceSpanType {_spanStart = s, _spanEnd = e}))     = (s, e)
  spanOf (TRecord _ (SourceSpanType {_spanStart = s, _spanEnd = e}))    = (s, e)
  spanOf (TVar _ (SourceSpanType {_spanStart = s, _spanEnd = e}))       = (s, e)
