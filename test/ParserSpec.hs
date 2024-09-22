module ParserSpec
    ( spec
    ) where

import           Data.Bifunctor (first)
import           Data.Text      (Text, pack, unlines)
-- import           Text.Parsec (ParseError)
import           Parser
import           Test.Hspec

parse' :: Text -> Either Text [Declaration]
parse' = first (pack . show) <$> parse "test.mnml"

spec :: Spec
spec =
  describe "Parser" $ do
    describe "constructor" $ do
      it "parses constructor with no arguments" $ do
        parse' "main = None"
          `shouldBe` Right [ValueDecl "main" (EConstructor "None" [])]

      it "parses constructor with one argument" $ do
        parse' "main = Success({ body: \"hello world!\"})"
          `shouldBe` Right [ValueDecl "main" (EConstructor "Success" [ERecord [("body", ELit (LString "hello world!"))]])]

      it "parses constructor with multiple arguments" $ do
        parse' "main = BinOp(1, '*', 2)"
          `shouldBe` Right [ValueDecl "main" (EConstructor "BinOp" [ELit (LInt 1), ELit (LChar '*'), ELit (LInt 2)])]

    describe "records" $ do
      it "parses single" $ do
        parse' "main = {name: \"Jon\"}" `shouldBe` Right [ValueDecl "main" (ERecord [("name", ELit (LString "Jon"))])]

    describe "case" $ do
      it "handles constructors" $ do
        parse'
          ( Data.Text.unlines
              [ "main = case foo of",
                "Just(n) -> n",
                "None -> 5"
              ]
          )
          `shouldBe` Right
            [ ValueDecl
                "main"
                ( ECase
                    (EVar "foo")
                    [ (PConstructor "Just" [PVar "n"], EVar "n"),
                      (PConstructor "None" [], ELit (LInt 5))
                    ]
                )
            ]

    describe "binary expressions" $ do
      it "parses addition" $ do
        parse' "main = 5 + 3" `shouldBe` Right [ValueDecl "main" (EBinary Add (ELit (LInt 5)) (ELit (LInt 3)))]

      it "does left association" $ do
        parse' "main = 1 + 2 + 3"
          `shouldBe` Right [ValueDecl "main" (EBinary Add (EBinary Add (ELit (LInt 1)) (ELit (LInt 2))) (ELit (LInt 3)))]

      it "parses parens correctly" $ do
        parse' "main = (5 + 3) * 3 == 20 + 4"
          `shouldBe` Right
            [ ValueDecl
                "main"
                ( EBinary
                    Equals
                    (EBinary Mul (EBinary Add (ELit (LInt 5)) (ELit (LInt 3))) (ELit (LInt 3)))
                    (EBinary Add (ELit (LInt 20)) (ELit (LInt 4)))
                )
            ]
