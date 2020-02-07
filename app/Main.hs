module Main where

import System.Environment
import Parser
-- import Translator


main :: IO ()
main = do
  let
      inDirName = "resources/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs"]
--      outFileName = inDirName ++ "res.hs"
--  writeFile outFileName $ unlines ["import Peano", ""]  
  mapM_
    (\fileName -> do
        putStrLn "-----------------------------------------------------------------------------------"
        putStrLn $ fileName ++ "\n"
        input <- readFile fileName
        putStrLn . strDefAsts $ input
{-
        let ast    = defsAsts $ input
            output = unlines $ show <$> translator ast
        putStrLn output
        appendFile outFileName output
-}
    )
    inFileNames
