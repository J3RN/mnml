module MNML.AST.Span
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpan (..)
    , Spanned (..)
    , Type (..)
    ) where

import           Data.Text   (Text)
import           Text.Parsec (SourcePos)

data SourceSpan
  = SourceSpan
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      }
  deriving (Eq, Show)

data Declaration
  = TypeDecl Text [(Text, [Type])] SourceSpan -- "MyType = Foo(String) | Bar(Int, String)"
  | TypeAliasDecl Text Type SourceSpan -- "alias  {name: String} as User" or "alias Int as Price"
  | ValueDecl Text Expr SourceSpan
  deriving (Eq, Show)

data Expr
  = EVar Text SourceSpan
  | EConstructor Text SourceSpan -- Foo
  | ELit Literal SourceSpan
  | ELambda [Text] Expr SourceSpan -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] SourceSpan -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)] SourceSpan
  | EBinary Operator Expr Expr SourceSpan
  | ERecord [(Text, Expr)] SourceSpan
  | EList [Expr] SourceSpan
  deriving (Eq, Show)

data Literal
  = LInt Integer SourceSpan
  | LFloat Double SourceSpan
  | LChar Char SourceSpan
  | LString Text SourceSpan
  deriving (Eq, Show)

data Pattern
  = PVar Text SourceSpan
  | PDiscard SourceSpan -- _
  | PConstructor Text [Pattern] SourceSpan
  | PRecord [(Text, Pattern)] SourceSpan
  | PList [Pattern] SourceSpan
  | PLiteral Literal SourceSpan
  deriving (Eq, Show)

data Operator = Add | Sub | Mul | Div | And | Or | Equals
  deriving (Eq, Show)

data Type
  = TInt SourceSpan
  | TFloat SourceSpan
  | TChar SourceSpan
  | TString SourceSpan
  | TNamedType Text SourceSpan -- "User"
  | TList Type SourceSpan -- TInt
  | TFun [Type] Type SourceSpan -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type)] SourceSpan -- [("name", TString), ...]
  | TVar Text SourceSpan -- "a"
  deriving (Eq, Show)

class Spanned a where
  spanOf :: a -> SourceSpan

instance Spanned Declaration where
  spanOf (TypeDecl _ _ s)      = s
  spanOf (TypeAliasDecl _ _ s) = s
  spanOf (ValueDecl _ _ s)     = s

instance Spanned Expr where
  spanOf (EVar _ s)         = s
  spanOf (EConstructor _ s) = s
  spanOf (ELit _ s)         = s
  spanOf (ELambda _ _ s)    = s
  spanOf (EApp _ _ s)       = s
  spanOf (ECase _ _ s)      = s
  spanOf (EBinary _ _ _ s)  = s
  spanOf (ERecord _ s)      = s
  spanOf (EList _ s)        = s

instance Spanned Literal where
  spanOf (LInt _ s)    = s
  spanOf (LFloat _ s)  = s
  spanOf (LChar _ s)   = s
  spanOf (LString _ s) = s

instance Spanned Pattern where
  spanOf (PVar _ s)           = s
  spanOf (PDiscard s)         = s
  spanOf (PConstructor _ _ s) = s
  spanOf (PRecord _ s)        = s
  spanOf (PList _ s)          = s
  spanOf (PLiteral _ s)       = s

instance Spanned Type where
  spanOf (TInt s)         = s
  spanOf (TFloat s)       = s
  spanOf (TChar s)        = s
  spanOf (TString s)      = s
  spanOf (TNamedType _ s) = s
  spanOf (TList _ s)      = s
  spanOf (TFun _ _ s)     = s
  spanOf (TRecord _ s)    = s
  spanOf (TVar _ s)       = s
