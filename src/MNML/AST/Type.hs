module MNML.AST.Type
    ( Definition (..)
    , Expr (..)
    , Literal (..)
    , Operator (..)
    , Pattern (..)
    , SourceSpanType (..)
    , Type (..)
    , Typed (..)
    , sourceSpanTypeToSourceSpan
    ) where

import           Data.Text     (Text)
import           MNML.AST.Span (SourceSpan (..), Spanned (..))
import           MNML.Base     (QualifiedValueReference)
import qualified MNML.Type     as T
import           Text.Parsec   (SourcePos)

data SourceSpanType
  = SourceSpanType
      { _spanStart :: SourcePos
      , _spanEnd   :: SourcePos
      , _type      :: T.Type
      }
  deriving (Eq, Show)

data Definition
  = TypeDef Text [(Text, [Type])] SourceSpanType -- "MyType = Foo(String) | Bar(Int, String)"
  | TypeAliasDef Text Type SourceSpanType -- "alias  {name: String} as User" or "alias Int as Price"
  | ValueDef Text Expr SourceSpanType
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

sourceSpanTypeToSourceSpan :: SourceSpanType -> SourceSpan
sourceSpanTypeToSourceSpan (SourceSpanType {_spanStart = s, _spanEnd = e}) = SourceSpan {_spanStart = s, _spanEnd = e}

class Typed a where
  typeOf :: a -> T.Type

instance Typed Definition where
  typeOf (TypeDef _ _ (SourceSpanType {_type = t}))      = t
  typeOf (TypeAliasDef _ _ (SourceSpanType {_type = t})) = t
  typeOf (ValueDef _ _ (SourceSpanType {_type = t}))     = t

instance Spanned Definition where
  spanOf (TypeDef _ _ s)      = sourceSpanTypeToSourceSpan s
  spanOf (TypeAliasDef _ _ s) = sourceSpanTypeToSourceSpan s
  spanOf (ValueDef _ _ s)     = sourceSpanTypeToSourceSpan s

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
  spanOf (EVar _ s)         = sourceSpanTypeToSourceSpan s
  spanOf (EConstructor _ s) = sourceSpanTypeToSourceSpan s
  spanOf (ELit _ s)         = sourceSpanTypeToSourceSpan s
  spanOf (ELambda _ _ s)    = sourceSpanTypeToSourceSpan s
  spanOf (EApp _ _ s)       = sourceSpanTypeToSourceSpan s
  spanOf (ECase _ _ s)      = sourceSpanTypeToSourceSpan s
  spanOf (EBinary _ _ _ s)  = sourceSpanTypeToSourceSpan s
  spanOf (ERecord _ s)      = sourceSpanTypeToSourceSpan s
  spanOf (EList _ s)        = sourceSpanTypeToSourceSpan s

instance Typed Literal where
  typeOf (LInt _ (SourceSpanType {_type = t}))    = t
  typeOf (LFloat _ (SourceSpanType {_type = t}))  = t
  typeOf (LChar _ (SourceSpanType {_type = t}))   = t
  typeOf (LString _ (SourceSpanType {_type = t})) = t

instance Spanned Literal where
  spanOf (LInt _ s)    = sourceSpanTypeToSourceSpan s
  spanOf (LFloat _ s)  = sourceSpanTypeToSourceSpan s
  spanOf (LChar _ s)   = sourceSpanTypeToSourceSpan s
  spanOf (LString _ s) = sourceSpanTypeToSourceSpan s

instance Typed Pattern where
  typeOf (PVar _ (SourceSpanType {_type = t}))           = t
  typeOf (PDiscard (SourceSpanType {_type = t}))         = t
  typeOf (PConstructor _ _ (SourceSpanType {_type = t})) = t
  typeOf (PRecord _ (SourceSpanType {_type = t}))        = t
  typeOf (PList _ (SourceSpanType {_type = t}))          = t
  typeOf (PLiteral _ (SourceSpanType {_type = t}))       = t

instance Spanned Pattern where
  spanOf (PVar _ s)           = sourceSpanTypeToSourceSpan s
  spanOf (PDiscard s)         = sourceSpanTypeToSourceSpan s
  spanOf (PConstructor _ _ s) = sourceSpanTypeToSourceSpan s
  spanOf (PRecord _ s)        = sourceSpanTypeToSourceSpan s
  spanOf (PList _ s)          = sourceSpanTypeToSourceSpan s
  spanOf (PLiteral _ s)       = sourceSpanTypeToSourceSpan s

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
  spanOf (TInt s)         = sourceSpanTypeToSourceSpan s
  spanOf (TFloat s)       = sourceSpanTypeToSourceSpan s
  spanOf (TChar s)        = sourceSpanTypeToSourceSpan s
  spanOf (TString s)      = sourceSpanTypeToSourceSpan s
  spanOf (TNamedType _ s) = sourceSpanTypeToSourceSpan s
  spanOf (TList _ s)      = sourceSpanTypeToSourceSpan s
  spanOf (TFun _ _ s)     = sourceSpanTypeToSourceSpan s
  spanOf (TRecord _ s)    = sourceSpanTypeToSourceSpan s
  spanOf (TVar _ s)       = sourceSpanTypeToSourceSpan s
