module Main where

import System.Environment
import Parser
import Translator

main :: IO ()
main = do
--  fileNames <- getArgs
  let inFileNames = ["resources/mkappendo"]
      outFileName = "resources/appendo.hs"
  mapM_
    (\fileName -> do
        putStrLn fileName
        input <- readFile fileName
        putStrLn input
        putStrLn "-----------------------------------------------------------------------------------"
        putStrLn . strDefAsts $ input
        putStrLn "-----------------------------------------------------------------------------------"
        let ast = defAst $ input
        strTranslator ast
        maybe (print "Haskell program is not exist") (writeFile outFileName . show) $ translator ast
    )
    inFileNames
  putStrLn "-----------------------------------------------------------------------------------------"
  putStrLn "OK"
