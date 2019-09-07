module Main where

import System.Environment
import Parser
import Translator

main :: IO ()
main = do
  let
      inDirName = "resources/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs"]
      outFileName = "resources/1.hs"
  writeFile outFileName "import Peano\n\nmaxo1 = undefined\n\n"  
  mapM_
    (\fileName -> do
        putStrLn fileName
        input <- readFile fileName
        putStrLn . strDefAsts $ input
        putStrLn "-----------------------------------------------------------------------------------"
        let ast    = defsAsts $ input
            output = unlines $ show <$> translator ast
        putStrLn output
        appendFile outFileName output
    )
    inFileNames
  putStrLn "-----------------------------------------------------------------------------------------"
  putStrLn "OK"
