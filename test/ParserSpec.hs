module ParserSpec
    ( spec
    ) where

import           Data.Bifunctor (first)
import           Data.Text (Text, pack)
-- import           Text.Parsec (ParseError)
import           Parser
import           Test.Hspec

parse' :: Text -> Either Text [Declaration]
parse' = first (pack . show) <$> parse "test.mnml"

spec :: Spec
spec = describe "constructor" $ do
  it "parses constructor with one argument" $ do
    parse' "main = Success({ body: \"hello world!\"})" `shouldBe`
      Right [ValueDecl "main" (EConstructor "Success" [ERecord [("body", ELit (LString "hello world!"))]])]

  it "parses constructor with multiple arguments" $ do
    parse' "main = BinOp(1, '*', 2)" `shouldBe`
      Right [ValueDecl "main" (EConstructor "BinOp" [ELit (LInt 1), ELit (LChar '*'), ELit (LInt 2)])]
