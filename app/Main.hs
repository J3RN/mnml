module Main
    ( main
    ) where

import           Parser
import           System.Environment
import           Text.Parsec.Text   (parseFromFile)

main :: IO ()
main = do
  [fName] <- getArgs
  result <- parseFromFile Parser.mod fName
  putStrLn . show $ result
