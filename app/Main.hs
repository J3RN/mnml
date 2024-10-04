module Main
    ( main
    ) where

import           Control.Exception   (try)
import           Control.Monad.State (evalState)
import           Data.Either         (fromRight)
import qualified Data.Map            as Map
import           Data.Text           (Text, pack)
import qualified Data.Text.IO        as TIO
import           MNML                (CompilerState (..))
-- import           MNML.Inference
import           System.Directory    (listDirectory)
import           System.FilePath     (dropExtension, isExtensionOf)

main :: IO ()
main = do
  modules <- loadModules "."
  let state = (CompilerState {_stateSpans = Map.empty, _stateModules = modules})
      -- result = evalState (infer "test" "main") state
  -- print result
  pure ()

loadModules :: FilePath -> IO (Map.Map Text Text)
loadModules basePath = do
  projFiles <- findSourceFiles basePath
  Map.fromList <$> mapM (\file -> (pack . dropExtension $ file,) <$> TIO.readFile file) projFiles

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles basePath = do
  files <- fromRight [] <$> ((try $ listDirectory basePath) :: IO (Either IOError [FilePath]))
  (filter (isExtensionOf "mnml") files ++) . concat <$> mapM findSourceFiles files
