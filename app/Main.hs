module Main (
      main
    ) where

import           Program.Prop
import           Syntax

import           Init


-----------------------------------------------------------------------------------------------------

initModule :: FilePath -> FilePath -> IO FilePath
initModule outDirName moduleName = do
  let outFileName = outDirName ++ moduleName ++ ".hs"
      moduleHead = unwords ["module", moduleName, "where"]
      imports = ["\n", "import Lib.Peano", "import Lib.Generator", "\n"]
  writeFile outFileName . unlines $ moduleHead : imports
  return $ outFileName

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let inDirName   = "resources/"
      outDirName  = "test/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs", "extra", "permSort"]
  nameToDef <- initDefsByNames inFileNames
 
-----------------------------------------------------------------------------------------------------
-- Appendo
-----------------------------------------------------------------------------------------------------

  appendoFileName <- initModule outDirName "Appendo"
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]

  let appendoIIO = mkToHsText appendoProgram ["x", "y"]
  putStrLn appendoIIO
  appendFile appendoFileName appendoIIO

  let appendoOOI = mkToHsText appendoProgram ["xy"]
  putStrLn appendoOOI
  appendFile appendoFileName appendoOOI

  let appendoIOO = mkToHsText appendoProgram ["x"]
  putStrLn appendoIOO
  appendFile appendoFileName appendoIOO

  let appendoOII = mkToHsText appendoProgram ["y", "xy"]
  putStrLn appendoOII
  appendFile appendoFileName appendoOII

  let appendoOIO = mkToHsText appendoProgram ["y"]
  putStrLn appendoOIO
  appendFile appendoFileName appendoOIO

  let appendoIOI = mkToHsText appendoProgram ["x", "xy"]
  putStrLn appendoIOI
  appendFile appendoFileName appendoIOI
{-
  let appendoIII = mkToHsText appendoProgram ["x", "y", "xy"]
  putStrLn appendoIII
  appendFile appendoFileName appendoIII

  let appendoOOO = mkToHsText appendoProgram []
  putStrLn appendoOOO
  appendFile appendoFileName appendoOOO
-}
-----------------------------------------------------------------------------------------------------

