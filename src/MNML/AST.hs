module MNML.AST
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpan (..)
    , SpanAnnotation (..)
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
