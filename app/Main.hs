module Main where

import System.Environment
import Parser

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        putStrLn fileName
        input <- readFile fileName
        putStrLn input
        putStrLn "-----------------------------------------------------------"
        putStrLn . getStrAstWithDefGoal $ input
        putStrLn "-----------------------------------------------------------"
    )
    fileNames
  putStrLn "-----------------------------------------------------------"
  putStrLn "OK"

