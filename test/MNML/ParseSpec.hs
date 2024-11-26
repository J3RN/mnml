module MNML.ParseSpec
    ( spec
    ) where

import           Control.Monad.State (runState)
import           Data.Bifunctor      (first)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           MNML                (CompilerState (..), emptyState)
import           MNML.AST.Span
import           MNML.Parse
import           SpecHelpers
import           Test.Hspec

parse' :: Text -> (Either Text [Declaration], CompilerState)
parse' source =
  first
    (first (Text.pack . show))
    (runState (moduleDef "test") (emptyState {_modules = Map.fromList [("test", source)]}))

spec :: Spec
spec =
  describe "Parser" $ do
    describe "constructor" $ do
      it "parses constructor with no arguments" $ do
        let (ast, _cs) = parse' "main = None"
        case ast of
          Right [ValueDecl "main" cons@(EConstructor "None" _) _] -> nodeShouldSpan cons (1, 8) (1, 12)
          other -> unexpected other

      it "parses constructor with one argument" $ do
        let (ast, _cs) = parse' "main = Success({ body: \"hello world!\"})"
        case ast of
          Right
            [ ValueDecl
                "main"
                app@( EApp
                        cons@(EConstructor "Success" _)
                        [ERecord [("body", ELit (LString "hello world!" _) _)] _]
                        _
                      )
                _
              ] -> nodeShouldSpan app (1, 8) (1, 40) >> nodeShouldSpan cons (1, 8) (1, 15)
          other -> unexpected other

      it "parses constructor with multiple arguments" $ do
        let (ast, _cs) = parse' "main = BinOp(1, '*', 2)"
        case ast of
          Right
            [ ValueDecl
                "main"
                app@(EApp cons@(EConstructor "BinOp" _) [ELit (LInt 1 _) _, ELit (LChar '*' _) _, ELit (LInt 2 _) _] _)
                _
              ] -> nodeShouldSpan app (1, 8) (1, 24) >> nodeShouldSpan cons (1, 8) (1, 13)
          other -> unexpected other

    describe "records" $ do
      it "parses single" $ do
        let (ast, _cs) = parse' "main = {name: \"Jon\"}"
        case ast of
          Right [ValueDecl "main" rec@(ERecord [("name", ELit (LString "Jon" _) _)] _) _] -> nodeShouldSpan rec (1, 8) (1, 21)
          other -> unexpected other

      it "parses two" $ do
        let (ast, _cs) = parse' "main =  {name: \"Jon\", age: 30}"
        case ast of
          Right
            [ ValueDecl "main" rec@(ERecord [("name", ELit (LString "Jon" _) _), ("age", ELit (LInt 30 _) _)] _) _
              ] -> nodeShouldSpan rec (1, 9) (1, 31)
          other -> unexpected other

    describe "case" $ do
      it "handles constructors" $ do
        let (ast, _cs) = parse' (Text.unlines ["main = case foo of", "Just(n) -> n", "None -> 5"])
        case ast of
          Right
            [ ValueDecl
                "main"
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
        let (ast, _cs) = parse' "main = 5 + 3"
        case ast of
          Right [ValueDecl "main" bin@(EBinary (Add) (ELit (LInt 5 _) _) (ELit (LInt 3 _) _) _) _] -> nodeShouldSpan bin (1, 8) (1, 13)
          other -> unexpected other

      it "does left association" $ do
        let (ast, _cs) = parse' "main = 1 + 2 + 3"
        case ast of
          Right
            [ ValueDecl
                "main"
                outer@(EBinary Add inner@(EBinary Add (ELit (LInt 1 _) _) (ELit (LInt 2 _) _) _) (ELit (LInt 3 _) _) _)
                _
              ] -> nodeShouldSpan outer (1, 8) (1, 17) >> nodeShouldSpan inner (1, 8) (1, 14)
          other -> unexpected other

      it "parses parens correctly" $ do
        let (ast, _cs) = parse' "main = (5 + 3) * 3 == 20 + 4"
        case ast of
          Right
            [ ValueDecl
                "main"
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
        let (ast, _cs) = parse' "main = foo(1)"
        case ast of
          Right [ValueDecl "main" app@(EApp (EVar "foo" _) [ELit (LInt 1 _) _] _) _] -> nodeShouldSpan app (1, 8) (1, 14)
          other -> unexpected other

      it "handles chained applications" $ do
        let (ast, _cs) = parse' "main = foo(1)(2)"
        case ast of
          Right
            [ ValueDecl
                "main"
                outer@(EApp inner@(EApp (EVar "foo" _) [ELit (LInt 1 _) _] _) [ELit (LInt 2 _) _] _)
                _
              ] ->
              nodeShouldSpan inner (1, 8) (1, 14)
                >> nodeShouldSpan outer (1, 8) (1, 17)
          other -> unexpected other

    describe "list literals" $ do
      it "handles empty list" $ do
        let (ast, _cs) = parse' "main = []"
        case ast of
          Right [ValueDecl "main" list@(EList [] _) _] -> nodeShouldSpan list (1, 8) (1, 10)
          other -> unexpected other

      it "handles singleton list" $ do
        let (ast, _cs) = parse' "main = [3.14]"
        case ast of
          Right [ValueDecl "main" list@(EList [ELit (LFloat 3.14 _) _] _) _] -> nodeShouldSpan list (1, 8) (1, 14)
          other -> unexpected other

      it "handles multiple list" $ do
        let (ast, _cs) = parse' "main = [3.14, 2.72]"
        case ast of
          Right [ValueDecl "main" list@(EList [ELit (LFloat 3.14 _) _, ELit (LFloat 2.72 _) _] _) _] -> nodeShouldSpan list (1, 8) (1, 20)
          other -> unexpected other
