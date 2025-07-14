module Main
    ( main
    ) where

import           Control.Exception   (try)
import           Control.Monad.State (evalState)
import           Data.Either         (fromRight)
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Text.IO        as TIO
import           MNML.CompilerState  (CompilerState (..), emptyState)
import           System.Directory    (listDirectory)
import           System.FilePath     (dropExtension, isExtensionOf)

main :: IO ()
main = do
  putStrLn "Hello, World!"
