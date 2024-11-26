module SpecHelpers
    ( nodeShouldSpan
    , shouldSpan
    , unexpected
    ) where

import           Control.Monad (unless)
import           MNML.AST.Span (SourceSpan (..), Spanned (..))
import           Test.Hspec
import           Text.Parsec   (sourceColumn, sourceLine)

unexpected :: (HasCallStack, Show a) => a -> Expectation
unexpected node = expectationFailure $ "Did not expect " <> show node

nodeShouldSpan :: (HasCallStack, Spanned a) => a -> (Int, Int) -> (Int, Int) -> Expectation
nodeShouldSpan node = shouldSpan (spanOf node)

shouldSpan :: (HasCallStack) => SourceSpan -> (Int, Int) -> (Int, Int) -> Expectation
shouldSpan (SourceSpan {_spanStart = sBeg, _spanEnd = sEnd}) beg end =
  let actualBeg = (sourceLine sBeg, sourceColumn sBeg)
      actualEnd = (sourceLine sEnd, sourceColumn sEnd)
   in unless
        (actualBeg == beg && actualEnd == end)
        ( expectationFailure
            ( concat
                [ "Expected span from "
                , show beg
                , " to "
                , show end
                , ", but actually spanned from "
                , show actualBeg
                , " to "
                , show actualEnd
                ]
            )
        )

