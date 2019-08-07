module Main where

import System.Environment
import Parser
import Translator

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        putStrLn fileName
        input <- readFile fileName
        putStrLn input
        putStrLn "-----------------------------------------------------------------------------------"
        putStrLn . strDefAsts $ input
        putStrLn "-----------------------------------------------------------------------------------"
        strTranslator . defAst $ input
    )
    fileNames
  putStrLn "-----------------------------------------------------------------------------------------"
  putStrLn "OK"

