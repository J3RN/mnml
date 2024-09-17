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
