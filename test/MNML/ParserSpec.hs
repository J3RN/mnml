module MNML.ParserSpec
    ( spec
    ) where

import           Control.Monad       (unless)
import           Control.Monad.State (runState)
import           Data.Bifunctor      (first)
import qualified Data.Map            as Map
import           Data.Text           (Text, pack, unlines)
import           MNML                (CompilerState (..), NodeId,
                                      SourceSpan (..))
import           MNML.AST
import           MNML.Parser
import           Test.Hspec
import           Text.Parsec         (sourceColumn)

parse' :: Text -> (Either Text [Declaration], CompilerState)
parse' source = first (first (pack . show)) (runState (parse "test.mnml" source) compilerState)
  where
    compilerState = CompilerState {_stateSpans = Map.empty, _stateModules = Map.empty, _stateTypes = Map.empty}

spec :: Spec
spec =
  describe "Parser" $ do
    describe "constructor" $ do
      it "parses constructor with no arguments" $ do
        let (ast, _cs) = parse' "main = None"
        ast `shouldBe` Right [ValueDecl "main" (EConstructor "None" 1) 0]

      it "tracks bare constructor span correctly" $ do
        let (ast, cs) = parse' "main = None"
            Right [ValueDecl _ (EConstructor "None" cId) _] = ast
        shouldSpan cs cId 8 12

      it "parses constructor with one argument" $ do
        let (ast, _cs) = parse' "main = Success({ body: \"hello world!\"})"
        ast `shouldBe` Right [ValueDecl "main" (EApp (EConstructor "Success" 1) [ERecord [("body", ELit (LString "hello world!" 4) 3)] 2] 5) 0]

      it "tracks constructor with one argument spans correctly" $ do
        let (ast, cs) = parse' "main = Success({ body: \"hello world!\"})"
            Right [ValueDecl _ (EApp (EConstructor "Success" cId) [ERecord [("body", ELit (LString "hello world!" _) _)] _] appId) _] = ast
        shouldSpan cs appId 8 40
        shouldSpan cs cId 8 15

      it "parses constructor with multiple arguments" $ do
        let (ast, _cs) = parse' "main = BinOp(1, '*', 2)"
        ast `shouldBe` Right [ValueDecl "main" (EApp (EConstructor "BinOp" 1) [ELit (LInt 1 3) 2, ELit (LChar '*' 5) 4, ELit (LInt 2 7) 6] 8) 0]

      it "tracks constructor with multiple arguments spans correctly" $ do
        let (ast, cs) = parse' "main =  BinOp(1, '*', 2)"
            Right [ValueDecl "main" (EApp (EConstructor "BinOp" cId) [ELit (LInt 1 _) _, ELit (LChar '*' _) _, ELit (LInt 2 _) _] appId) _] = ast
        shouldSpan cs appId 9 25
        shouldSpan cs cId 9 14

    describe "records" $ do
      it "parses single" $ do
        let (ast, _cs) = parse' "main = {name: \"Jon\"}"
        ast `shouldBe` Right [ValueDecl "main" (ERecord [("name", ELit (LString "Jon" 3) 2)] 1) 0]

      it "tracks single-item spans correctly" $ do
        let (ast, cs) = parse' "main = {name: \"Jon\"}"
            Right [ValueDecl "main" (ERecord [("name", ELit (LString "Jon" _) _)] recId) _] = ast
        shouldSpan cs recId 8 21

      it "parses two" $ do
        let (ast, _cs) = parse' "main =  {name: \"Jon\", age: 30}"
        ast `shouldBe` Right [ValueDecl "main" (ERecord [("name", ELit (LString "Jon" 3) 2), ("age", ELit (LInt 30 5) 4)] 1) 0]

      it "tracks single-item spans correctly" $ do
        let (ast, cs) = parse' "main =  {name: \"Jon\", age: 30}"
            Right [ValueDecl "main" (ERecord [("name", _), ("age", _)] recId) _] = ast
        shouldSpan cs recId 9 31

    describe "case" $ do
      it "handles constructors" $ do
        let (ast, _cs) = parse' (Data.Text.unlines ["main = case foo of", "Just(n) -> n", "None -> 5"])
        ast
          `shouldBe` Right
            [ ValueDecl
                "main"
                ( ECase
                    (EVar "foo" 2)
                    [ (PConstructor "Just" [PVar "n" 4] 3, EVar "n" 5),
                      (PConstructor "None" [] 6, ELit (LInt 5 8) 7)
                    ]
                    1
                )
                0
            ]

    describe "binary expressions" $ do
      it "parses addition" $ do
        let (ast, _cs) = parse' "main = 5 + 3"
        ast `shouldBe` Right [ValueDecl "main" (EBinary (Add 4) (ELit (LInt 5 2) 1) (ELit (LInt 3 6) 5) 3) 0]

      it "tracks addition expression span correctly" $ do
        let (ast, cs) = parse' "main = 5 + 3"
            (Right [ValueDecl _ (EBinary (Add _) _ _ exprId) _]) = ast
        shouldSpan cs exprId 8 13

      it "does left association" $ do
        let (ast, _cs) = parse' "main = 1 + 2 + 3"
        ast `shouldBe` Right [ValueDecl "main" (EBinary (Add 8) (EBinary (Add 4) (ELit (LInt 1 2) 1) (ELit (LInt 2 6) 5) 3) (ELit (LInt 3 10) 9) 7) 0]

      it "tracks left association spans correctly" $ do
        let (ast, cs) = parse' "main = 1 + 2 + 3"
            (Right [ValueDecl _ (EBinary (Add _) (EBinary (Add _) _ _ innerId) _ outerId) _]) = ast
        shouldSpan cs outerId 8 17
        shouldSpan cs innerId 8 14

      it "parses parens correctly" $ do
        let (ast, _cs) = parse' "main = (5 + 3) * 3 == 20 + 4"
        ast
          `shouldBe` Right
            [ ValueDecl
                "main"
                ( EBinary
                    (Equals 12)
                    (EBinary (Mul 8) (EBinary (Add 4) (ELit (LInt 5 2) 1) (ELit (LInt 3 6) 5) 3) (ELit (LInt 3 10) 9) 7)
                    (EBinary (Add 16) (ELit (LInt 20 14) 13) (ELit (LInt 4 18) 17) 15)
                    11
                )
                0
            ]

      it "tracks paren spans correctly" $ do
        let (ast, cs) = parse' "main = (5 + 3) * 3 == 20 + 4"
            (Right [ValueDecl _ (EBinary (Equals _) (EBinary (Mul _) (EBinary (Add _) _ _ lAddId) _ mulId) (EBinary (Add _) _ _ rAddId) eqId) _]) = ast
        shouldSpan cs eqId 8 29
        shouldSpan cs mulId 8 20
        shouldSpan cs lAddId 9 14
        shouldSpan cs rAddId 23 29

    describe "function application" $ do
      it "handles single application" $ do
        let (ast, _cs) = parse' "main = foo(1)"
        ast `shouldBe` Right [ValueDecl "main" (EApp (EVar "foo" 1) [ELit (LInt 1 3) 2] 4) 0]

      it "tracks single application spans correctly" $ do
        let (ast, cs) = parse' "main = foo(1)"
            (Right [ValueDecl _ (EApp (EVar "foo" _) _args appId) _]) = ast
        shouldSpan cs appId 8 14

      it "handles chained applications" $ do
        let (ast, _cs) = parse' "main = foo(1)(2)"
        ast `shouldBe` Right [ValueDecl "main" (EApp (EApp (EVar "foo" 1) [ELit (LInt 1 3) 2] 6) [ELit (LInt 2 5) 4] 7) 0]

      it "tracks chained application spans correctly" $ do
        let (ast, cs) = parse' "main = foo(1)(2)"
            (Right [ValueDecl _ (EApp (EApp (EVar "foo" _) _ _) _ appId) _]) = ast
        shouldSpan cs appId 8 17

-- Custom expectation for spans

shouldSpan :: (HasCallStack) => CompilerState -> NodeId -> Int -> Int -> Expectation
shouldSpan (CompilerState {_stateSpans = spans}) nodeId beg end =
  let (SourceSpan sBeg sEnd) = spans Map.! nodeId
      actualBeg = sourceColumn sBeg
      actualEnd = sourceColumn sEnd
   in unless (actualBeg == beg && actualEnd == end) (expectationFailure $ mconcat ["Expected span from ", show beg, " to ", show end, ", but actually spanned from ", show actualBeg, " to ", show actualEnd])
