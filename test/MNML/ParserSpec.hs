module MNML.ParserSpec
    ( spec
    ) where

import           Control.Monad.State (runState)
import           Data.Bifunctor      (first)
import qualified Data.Map            as Map
import           Data.Text           (Text, pack, unlines)
import           MNML                (CompilerState (..))
import           MNML.Parser
import           Test.Hspec

parse' :: Text -> ((Either Text [Declaration]), CompilerState)
parse' source = first (first (pack . show)) (runState (parse "test.mnml" source) compilerState)
  where
    compilerState = CompilerState {_stateSpans = Map.empty, _stateModules = Map.empty, _stateTypes = Map.empty}

spec :: Spec
spec =
  describe "Parser" $ do
    -- describe "constructor" $ do
    --   it "parses constructor with no arguments" $ do
    --     parse' "main = None"
    --       `shouldBe` Right [ValueDecl "main" (EConstructor "None" 2) 1]

    --   it "parses constructor with one argument" $ do
    --     parse' "main = Success({ body: \"hello world!\"})"
    --       `shouldBe` Right [ValueDecl "main" (EApp (EConstructor "Success" 3) [ERecord [("body", ELit (LString "hello world!" 6) 5)] 4] 2) 1]

    --   it "parses constructor with multiple arguments" $ do
    --     parse' "main = BinOp(1, '*', 2)"
    --       `shouldBe` Right [ValueDecl "main" (EApp (EConstructor "BinOp") [ELit (LInt 1 4) 3, ELit (LChar '*' 6) 5, ELit (LInt 2 8) 7] 2) 1]

    describe "records" $ do
      it "parses single" $ do
        let (ast, _cs) = parse' "main = {name: \"Jon\"}"
        ast `shouldBe` Right [ValueDecl "main" (ERecord [("name", (ELit (LString "Jon" 3) 2))] 1) 0]

    describe "case" $ do
      it "handles constructors" $ do
        let (ast, _cs) = parse' (Data.Text.unlines ["main = case foo of", "Just(n) -> n", "None -> 5"])
        ast
          `shouldBe` Right
            [ ValueDecl
                "main"
                ( ECase
                    (EVar "foo" 2)
                    [ (PConstructor "Just" [PVar "n" 4] 3, (EVar "n" 5)),
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

-- it "does left association" $ do
--   let (ast, _cs) = parse' "main = 1 + 2 + 3"
--   ast `shouldBe` Right [ValueDecl "main" (EBinary Add (EBinary (Add 4) (ELit (LInt 1 3) 2) (ELit (LInt 2))) (ELit (LInt 3)) 1) 0]

-- it "parses parens correctly" $ do
--   let (ast, _cs) = parse' "main = (5 + 3) * 3 == 20 + 4"
--   ast
--     `shouldBe` Right
--       [ ValueDecl
--           "main"
--           ( EBinary
--               Equals
--               (EBinary Mul (EBinary Add (ELit (LInt 5)) (ELit (LInt 3))) (ELit (LInt 3)))
--               (EBinary Add (ELit (LInt 20)) (ELit (LInt 4)))
--           )
--       ]

-- describe "function application" $ do
--   it "handles single application" $ do
--     parse' "main = foo(1)" `shouldBe` Right [ValueDecl "main" (EApp (EVar "foo") [ELit (LInt 1)])]

--   it "handles chained applications" $ do
--     parse' "main = foo(1)(2)" `shouldBe` Right [ValueDecl "main" (EApp (EApp (EVar "foo") [ELit (LInt 1)]) [ELit (LInt 2)])]
