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

import           Data.Text   (Text)
import qualified MNML.Type   as T
import           Text.Parsec (SourcePos)

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

-- class ToTyped a where
--   setType :: a ->

class Typed a where
  typeOf :: a -> T.Type
