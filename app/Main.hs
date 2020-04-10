module Main where

import C.Parser
import C.Semantic
import C.Semantic.Checker
import Control.Monad.Except
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      fileContent <- readFile fileName
      case runParser parseFile fileName fileContent of
        Left err         -> putStrLn (show err)
        Right listOfCDef -> do
          env <- nullEnv
          checkFile env listOfCDef
