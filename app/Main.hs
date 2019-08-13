module Main where

import System.Environment
import Parser
import Translator

main :: IO ()
main = do
--  fileNames <- getArgs
  let
--      inFileNames = ["resources/mkappendo"]
      inFileNames = ["resources/list"]
      outFileName = "resources/prog.hs"
  mapM_
    (\fileName -> do
        putStrLn fileName
        input <- readFile fileName
 --       putStrLn input
 --       putStrLn "-----------------------------------------------------------------------------------"
        putStrLn . strDefAsts $ input
        putStrLn "-----------------------------------------------------------------------------------"
        let ast    = defsAsts $ input
            output = unlines $ show <$> translator ast
        putStrLn output
        writeFile outFileName output
    )
    inFileNames
  putStrLn "-----------------------------------------------------------------------------------------"
  putStrLn "OK"
