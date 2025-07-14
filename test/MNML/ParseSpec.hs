module MNML.ParseSpec
    ( spec
    ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           MNML.AST.Span
import           MNML.CompilerState   (emptyState)
import           MNML.Parse           (parse)
import           SpecHelpers
import           Test.Hspec

parse' :: Text -> Either Text [Definition]
parse' source =
  case evalState (runExceptT (parse source)) emptyState of
    Left errors -> Left (Text.pack (show errors))
    Right defs  -> Right defs

spec :: Spec
spec = do
  describe "constructor" $ do
    it "parses constructor with no arguments" $ do
      let batch = parse' "main = None"
      case batch of
        Right [ValueDef ([], "main") cons@(EConstructor "None" _) _] -> nodeShouldSpan cons (1, 8) (1, 12)
        other -> unexpected other

    it "parses constructor with one argument" $ do
      let batch = parse' "main = Success({ body: \"hello world!\"})"
      case batch of
        Right
          [ ValueDef
              ([], "main")
              app@( EApp
                      cons@(EConstructor "Success" _)
                      [ERecord [("body", ELit (LString "hello world!" _) _)] _]
                      _
                    )
              _
            ] -> nodeShouldSpan app (1, 8) (1, 40) >> nodeShouldSpan cons (1, 8) (1, 15)
        other -> unexpected other

    it "parses constructor with multiple arguments" $ do
      let batch = parse' "main = BinOp(1, '*', 2)"
      case batch of
        Right
          [ ValueDef
              ([], "main")
              app@(EApp cons@(EConstructor "BinOp" _) [ELit (LInt 1 _) _, ELit (LChar '*' _) _, ELit (LInt 2 _) _] _)
              _
            ] -> nodeShouldSpan app (1, 8) (1, 24) >> nodeShouldSpan cons (1, 8) (1, 13)
        other -> unexpected other

  describe "records" $ do
    it "parses single" $ do
      let batch = parse' "main = {name: \"Jon\"}"
      case batch of
        Right [ValueDef ([], "main") rec@(ERecord [("name", ELit (LString "Jon" _) _)] _) _] -> nodeShouldSpan rec (1, 8) (1, 21)
        other -> unexpected other

    it "parses two" $ do
      let batch = parse' "main =  {name: \"Jon\", age: 30}"
      case batch of
        Right
          [ ValueDef ([], "main") rec@(ERecord [("name", ELit (LString "Jon" _) _), ("age", ELit (LInt 30 _) _)] _) _
            ] -> nodeShouldSpan rec (1, 9) (1, 31)
        other -> unexpected other

  describe "case" $ do
    it "handles constructors" $ do
      let batch = parse' (Text.unlines ["main = case foo of", "Just(n) -> n", "None -> 5"])
      case batch of
        Right
          [ ValueDef
              ([], "main")
              c@( ECase
                    (EVar "foo" _)
                    [ (PConstructor "Just" [PVar "n" _] _, EVar "n" _)
                      , (PConstructor "None" [] _, ELit (LInt 5 _) _)
                      ]
                    _
                  )
              _
            ] -> nodeShouldSpan c (1, 8) (4, 1) -- A bit odd but I'm not questioning it
        other -> unexpected other

  describe "binary expressions" $ do
    it "parses addition" $ do
      let batch = parse' "main = 5 + 3"
      case batch of
        Right [ValueDef ([], "main") bin@(EBinary (Add) (ELit (LInt 5 _) _) (ELit (LInt 3 _) _) _) _] -> nodeShouldSpan bin (1, 8) (1, 13)
        other -> unexpected other

    it "does left association" $ do
      let batch = parse' "main = 1 + 2 + 3"
      case batch of
        Right
          [ ValueDef
              ([], "main")
              outer@(EBinary Add inner@(EBinary Add (ELit (LInt 1 _) _) (ELit (LInt 2 _) _) _) (ELit (LInt 3 _) _) _)
              _
            ] -> nodeShouldSpan outer (1, 8) (1, 17) >> nodeShouldSpan inner (1, 8) (1, 14)
        other -> unexpected other

    it "parses parens correctly" $ do
      let batch = parse' "main = (5 + 3) * 3 == 20 + 4"
      case batch of
        Right
          [ ValueDef
              ([], "main")
              eq@( EBinary
                    Equals
                    mul@(EBinary Mul lAdd@(EBinary Add (ELit (LInt 5 _) _) (ELit (LInt 3 _) _) _) (ELit (LInt 3 _) _) _)
                    rAdd@(EBinary Add (ELit (LInt 20 _) _) (ELit (LInt 4 _) _) _)
                    _
                  )
              _
            ] ->
            nodeShouldSpan eq (1, 8) (1, 29)
              >> nodeShouldSpan mul (1, 8) (1, 20)
              >> nodeShouldSpan lAdd (1, 9) (1, 14)
              >> nodeShouldSpan rAdd (1, 23) (1, 29)
        other -> unexpected other

  describe "function application" $ do
    it "handles single application" $ do
      let batch = parse' "main = foo(1)"
      case batch of
        Right [ValueDef ([], "main") app@(EApp (EVar "foo" _) [ELit (LInt 1 _) _] _) _] -> nodeShouldSpan app (1, 8) (1, 14)
        other -> unexpected other

    it "handles chained applications" $ do
      let batch = parse' "main = foo(1)(2)"
      case batch of
        Right
          [ ValueDef
              ([], "main")
              outer@(EApp inner@(EApp (EVar "foo" _) [ELit (LInt 1 _) _] _) [ELit (LInt 2 _) _] _)
              _
            ] ->
            nodeShouldSpan inner (1, 8) (1, 14)
              >> nodeShouldSpan outer (1, 8) (1, 17)
        other -> unexpected other

  describe "list literals" $ do
    it "handles empty list" $ do
      let batch = parse' "main = []"
      case batch of
        Right [ValueDef ([], "main") list@(EList [] _) _] -> nodeShouldSpan list (1, 8) (1, 10)
        other -> unexpected other

    it "handles singleton list" $ do
      let batch = parse' "main = [3.14]"
      case batch of
        Right [ValueDef ([], "main") list@(EList [ELit (LFloat 3.14 _) _] _) _] -> nodeShouldSpan list (1, 8) (1, 14)
        other -> unexpected other

    it "handles multiple list" $ do
      let batch = parse' "main = [3.14, 2.72]"
      case batch of
        Right [ValueDef ([], "main") list@(EList [ELit (LFloat 3.14 _) _, ELit (LFloat 2.72 _) _] _) _] -> nodeShouldSpan list (1, 8) (1, 20)
        other -> unexpected other

  describe "type definitions" $ do
    it "parses simple type with no constructors" $ do
      let batch = parse' "Maybe = Just(Int) | None"
      case batch of
        Right [TypeDef ([], "Maybe") [Constructor "Just" [TInt _] _, Constructor "None" [] _] _] -> pure ()
        other -> unexpected other

    it "parses type with constructor that has multiple arguments" $ do
      let batch = parse' "Result = Success(String) | Error(Int, String)"
      case batch of
        Right
          [ TypeDef ([], "Result")
              [ Constructor "Success" [TString _] _
              , Constructor "Error" [TInt _, TString _] _
              ] _
          ] -> pure ()
        other -> unexpected other

    it "parses qualified type definition" $ do
      let batch = parse' "my_app::Result = Success(String) | Error(String)"
      case batch of
        Right
          [ TypeDef (["my_app"], "Result")
              [ Constructor "Success" [TString _] _
              , Constructor "Error" [TString _] _
              ] _
          ] -> pure ()
        other -> unexpected other

    it "parses type with complex nested types" $ do
      let batch = parse' "Tree = Leaf(Int) | Branch(Tree, Tree)"
      case batch of
        Right
          [ TypeDef ([], "Tree")
              [ Constructor "Leaf" [TInt _] _
              , Constructor "Branch" [TNamedType "Tree" _, TNamedType "Tree" _] _
              ] _
          ] -> pure ()
        other -> unexpected other

    it "parses type with list types" $ do
      let batch = parse' "Container = Full([Int]) | Empty"
      case batch of
        Right
          [ TypeDef ([], "Container")
              [ Constructor "Full" [TList (TInt _) _] _
              , Constructor "Empty" [] _
              ] _
          ] -> pure ()
        other -> unexpected other

    it "parses type with function types" $ do
      let batch = parse' "Handler = Create((Int) -> String) | Update((Int, Int) -> String)"
      case batch of
        Right
          [ TypeDef ([], "Handler")
              [ Constructor "Create" [TFun [TInt _] (TString _) _] _
              , Constructor "Update" [TFun [TInt _, TInt _] (TString _) _] _
              ] _
          ] -> pure ()
        other -> unexpected other

    it "parses type with record types" $ do
      let batch = parse' "Entity = User({name: String, age: Int}) | Admin({permissions: [String]})"
      case batch of
        Right
          [ TypeDef ([], "Entity")
              [ Constructor "User" [TRecord [("name", TString _), ("age", TInt _)] _] _
              , Constructor "Admin" [TRecord [("permissions", TList (TString _) _)] _] _
              ] _
          ] -> pure ()
        other -> unexpected other

  describe "type alias definitions" $ do
    it "parses simple type alias definition" $ do
      let batch = parse' "alias String as Name"
      case batch of
        Right [TypeAliasDef ([], "Name") (TString _) _] -> pure ()
        other                                           -> unexpected other

    it "parses qualified type alias definition" $ do
      let batch = parse' "alias String as my_app::UserName"
      case batch of
        Right [TypeAliasDef (["my_app"], "UserName") (TString _) _] -> pure ()
        other -> unexpected other

  describe "qualified" $ do
    it "parses first-level module value definition" $ do
      let batch = parse' "my_app::test = 3.14"
      case batch of
        Right [def@(ValueDef (["my_app"], "test") (ELit (LFloat 3.14 _) _) _)] -> nodeShouldSpan def (1, 1) (1, 20)
        other -> unexpected other

    it "parses multi-level module value definition" $ do
      let batch = parse' "my_app/foo/bar::baz = 3.14"
      case batch of
        Right [def@(ValueDef (["my_app", "foo", "bar"], "baz") (ELit (LFloat 3.14 _) _) _)] -> nodeShouldSpan def (1, 1) (1, 27)
        other -> unexpected other
