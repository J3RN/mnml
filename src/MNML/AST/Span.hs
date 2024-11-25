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
  spanOf :: a -> (SourcePos, SourcePos)

instance Spanned Declaration where
  spanOf (TypeDecl _ _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (TypeAliasDecl _ _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ValueDecl _ _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)

instance Spanned Expr where
  spanOf (EVar _ (SourceSpan {_spanStart = s, _spanEnd = e}))         = (s, e)
  spanOf (EConstructor _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (ELit _ (SourceSpan {_spanStart = s, _spanEnd = e}))         = (s, e)
  spanOf (ELambda _ _ (SourceSpan {_spanStart = s, _spanEnd = e}))    = (s, e)
  spanOf (EApp _ _ (SourceSpan {_spanStart = s, _spanEnd = e}))       = (s, e)
  spanOf (ECase _ _ (SourceSpan {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (EBinary _ _ _ (SourceSpan {_spanStart = s, _spanEnd = e}))  = (s, e)
  spanOf (ERecord _ (SourceSpan {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (EList _ (SourceSpan {_spanStart = s, _spanEnd = e}))        = (s, e)

instance Spanned Literal where
  spanOf (LInt _ (SourceSpan {_spanStart = s, _spanEnd = e}))    = (s, e)
  spanOf (LFloat _ (SourceSpan {_spanStart = s, _spanEnd = e}))  = (s, e)
  spanOf (LChar _ (SourceSpan {_spanStart = s, _spanEnd = e}))   = (s, e)
  spanOf (LString _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)

instance Spanned Pattern where
  spanOf (PVar _ (SourceSpan {_spanStart = s, _spanEnd = e}))           = (s, e)
  spanOf (PDiscard (SourceSpan {_spanStart = s, _spanEnd = e}))         = (s, e)
  spanOf (PConstructor _ _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (PRecord _ (SourceSpan {_spanStart = s, _spanEnd = e}))        = (s, e)
  spanOf (PList _ (SourceSpan {_spanStart = s, _spanEnd = e}))          = (s, e)
  spanOf (PLiteral _ (SourceSpan {_spanStart = s, _spanEnd = e}))       = (s, e)

instance Spanned Type where
  spanOf (TInt (SourceSpan {_spanStart = s, _spanEnd = e}))         = (s, e)
  spanOf (TFloat (SourceSpan {_spanStart = s, _spanEnd = e}))       = (s, e)
  spanOf (TChar (SourceSpan {_spanStart = s, _spanEnd = e}))        = (s, e)
  spanOf (TString (SourceSpan {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (TNamedType _ (SourceSpan {_spanStart = s, _spanEnd = e})) = (s, e)
  spanOf (TList _ (SourceSpan {_spanStart = s, _spanEnd = e}))      = (s, e)
  spanOf (TFun _ _ (SourceSpan {_spanStart = s, _spanEnd = e}))     = (s, e)
  spanOf (TRecord _ (SourceSpan {_spanStart = s, _spanEnd = e}))    = (s, e)
  spanOf (TVar _ (SourceSpan {_spanStart = s, _spanEnd = e}))       = (s, e)
