module Main
    ( main
    ) where

import           Control.Exception (try)
import           Data.Either       (fromRight)
import qualified Data.Map          as Map
import           Data.Text         (Text, pack)
import qualified Data.Text.IO      as TIO
import           Inference
import           System.Directory  (listDirectory)
import           System.FilePath   (dropExtension, isExtensionOf)

main :: IO ()
main = do
  modules <- loadModules "."
  let result = infer modules "test" "main"
  print result

loadModules :: FilePath -> IO (Map.Map Text Text)
loadModules basePath = do
  projFiles <- findSourceFiles basePath
  Map.fromList <$> mapM (\file -> (pack . dropExtension $ file,) <$> TIO.readFile file) projFiles

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles basePath = do
  files <- fromRight [] <$> ((try $ listDirectory basePath) :: IO (Either IOError [FilePath]))
  (filter (isExtensionOf "mnml") files ++) . concat <$> mapM findSourceFiles files
