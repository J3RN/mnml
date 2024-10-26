module MNML.AST
    ( Declaration (..)
    , Expr (..)
    , Literal (..)
    , NodeId
    , Operator (..)
    , Pattern (..)
    , Type (..)
    ) where

import           Data.Text (Text)

type NodeId = Integer

data Declaration
  = TypeDecl Text [(Text, [Type])] NodeId -- "MyType = Foo(String) | Bar(Int, String)"
  | TypeAliasDecl Text Type NodeId -- "alias User = {name: String}" or "alias Price = Int"
  | ValueDecl Text Expr NodeId
  deriving (Eq, Show)

data Expr
  = EVar Text NodeId
  | EConstructor Text NodeId -- Foo
  | ELit Literal NodeId
  | ELambda [Text] Expr NodeId -- ["x", "y"] -> EBinary (EVar "x") Add (EVar "y")
  | EApp Expr [Expr] NodeId -- (EVar "fun") [(EVar "x"), (EVar "y")]
  | ECase Expr [(Pattern, Expr)] NodeId
  | EBinary Operator Expr Expr NodeId
  | ERecord [(Text, Expr)] NodeId
  | EList [Expr] NodeId
  deriving (Eq, Show)

data Literal
  = LInt Integer NodeId
  | LFloat Double NodeId
  | LChar Char NodeId
  | LString Text NodeId
  deriving (Eq, Show)

data Pattern
  = PVar Text NodeId
  | PDiscard NodeId -- _
  | PConstructor Text [Pattern] NodeId
  | PRecord [(Text, Pattern)] NodeId
  | PList [Pattern] NodeId
  | PLiteral Literal NodeId
  deriving (Eq, Show)

data Operator
  = Add NodeId
  | Sub NodeId
  | Mul NodeId
  | Div NodeId
  | And NodeId
  | Or NodeId
  | Equals NodeId
  deriving (Eq, Show)

data Type
  = TInt NodeId
  | TFloat NodeId
  | TChar NodeId
  | TString NodeId
  | TNamedType Text NodeId -- "User"
  | TList Type NodeId -- TInt
  | TFun [Type] Type NodeId -- [TInt, TInt] -> TInt
  | TRecord [(Text, Type)] NodeId -- [("name", TString), ...]
  | TVar Text NodeId -- "a"
  deriving (Eq, Show)
