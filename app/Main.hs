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
-- PermSort
-----------------------------------------------------------------------------------------------------

  permSortFileName <- initModule outDirName "PermSort"
  let permSortProgram = createProgram nameToDef "permSort" ["l", "sorted"]

  let permSortIO = mkToHsText permSortProgram ["l"]
  putStrLn permSortIO
  appendFile permSortFileName permSortIO

  let permSortOI = mkToHsText permSortProgram ["sorted"]
  putStrLn permSortOI
  appendFile permSortFileName permSortOI

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
-- Revacco
-----------------------------------------------------------------------------------------------------

  revaccoFileName <- initModule outDirName "Revacco"
  let revaccoProgram = createProgram nameToDef "revacco" ["xs", "acc", "sx"]

  let revaccoIOO = mkToHsText revaccoProgram ["xs"]
  putStrLn revaccoIOO
  appendFile revaccoFileName revaccoIOO

  let revaccoOII = mkToHsText revaccoProgram ["acc", "sx"]
  putStrLn revaccoOII
  appendFile revaccoFileName revaccoOII

  let revaccoOIO = mkToHsText revaccoProgram ["acc"]
  putStrLn revaccoOIO
  appendFile revaccoFileName revaccoOIO

  let revaccoIOI = mkToHsText revaccoProgram ["xs", "sx"]
  putStrLn revaccoIOI
  appendFile revaccoFileName revaccoIOI

  let revaccoOOI = mkToHsText revaccoProgram ["sx"]
  putStrLn revaccoOOI
  appendFile revaccoFileName revaccoOOI

  let revaccoIIO = mkToHsText revaccoProgram ["xs", "acc"]
  putStrLn revaccoIIO
  appendFile revaccoFileName revaccoIIO

  let revaccoIII = mkToHsText revaccoProgram ["xs", "acc", "sx"]
  putStrLn revaccoIII
  appendFile revaccoFileName revaccoIII

  let revaccoOOO = mkToHsText revaccoProgram []
  putStrLn revaccoOOO
  appendFile revaccoFileName revaccoOOO

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
-- Reverso
-----------------------------------------------------------------------------------------------------

  reversoFileName <- initModule outDirName "Reverso"
  let reversoProgram = createProgram nameToDef "reverso" ["x", "y"]

  let reversoIO = mkToHsText reversoProgram ["x"]
  putStrLn reversoIO
  appendFile reversoFileName reversoIO

  let reversoIO = mkToHsText reversoProgram ["y"]
  putStrLn reversoIO
  appendFile reversoFileName reversoIO

  let reversoII = mkToHsText reversoProgram ["x", "y"]
  putStrLn reversoII
  appendFile reversoFileName reversoII

  let reversoOO = mkToHsText reversoProgram []
  putStrLn reversoOO
  appendFile reversoFileName reversoOO

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
-- DoubleAppendo
-----------------------------------------------------------------------------------------------------

  doubleAppendoFileName <- initModule outDirName "DoubleAppendo"
  let doubleAppendoProgram = createProgram nameToDef "doubleAppendo" ["x", "xx"]

  let doubleAppendoIO = mkToHsText doubleAppendoProgram ["x"]
  putStrLn doubleAppendoIO
  appendFile doubleAppendoFileName doubleAppendoIO

  let doubleAppendoOI = mkToHsText doubleAppendoProgram ["xx"]
  putStrLn doubleAppendoOI
  appendFile doubleAppendoFileName doubleAppendoOI

  let doubleAppendoII = mkToHsText doubleAppendoProgram ["x", "xx"]
  putStrLn doubleAppendoII
  appendFile doubleAppendoFileName doubleAppendoII

  let doubleAppendoOO = mkToHsText doubleAppendoProgram []
  putStrLn doubleAppendoOO
  appendFile doubleAppendoFileName doubleAppendoOO

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
-- AppendoCtorUnif
-----------------------------------------------------------------------------------------------------

  appendoCtorUnifFileName <- initModule outDirName "AppendoCtorUnif"
  let appendoCtorUnifProgram = createProgram nameToDef "appendoCtorUnif" ["x", "y", "xy"]

  let appendoCtorUnifIIO = mkToHsText appendoCtorUnifProgram ["x", "y"]
  putStrLn appendoCtorUnifIIO
  appendFile appendoCtorUnifFileName appendoCtorUnifIIO

  let appendoCtorUnifOOI = mkToHsText appendoCtorUnifProgram ["xy"]
  putStrLn appendoCtorUnifOOI
  appendFile appendoCtorUnifFileName appendoCtorUnifOOI

  let appendoCtorUnifIOO = mkToHsText appendoCtorUnifProgram ["x"]
  putStrLn appendoCtorUnifIOO
  appendFile appendoCtorUnifFileName appendoCtorUnifIOO

  let appendoCtorUnifOII = mkToHsText appendoCtorUnifProgram ["y", "xy"]
  putStrLn appendoCtorUnifOII
  appendFile appendoCtorUnifFileName appendoCtorUnifOII

  let appendoCtorUnifOIO = mkToHsText appendoCtorUnifProgram ["y"]
  putStrLn appendoCtorUnifOIO
  appendFile appendoCtorUnifFileName appendoCtorUnifOIO

  let appendoCtorUnifIOI = mkToHsText appendoCtorUnifProgram ["x", "xy"]
  putStrLn appendoCtorUnifIOI
  appendFile appendoCtorUnifFileName appendoCtorUnifIOI

  let appendoCtorUnifIII = mkToHsText appendoCtorUnifProgram ["x", "y", "xy"]
  putStrLn appendoCtorUnifIII
  appendFile appendoCtorUnifFileName appendoCtorUnifIII

  let appendoCtorUnifOOO = mkToHsText appendoCtorUnifProgram []
  putStrLn appendoCtorUnifOOO
  appendFile appendoCtorUnifFileName appendoCtorUnifOOO

-----------------------------------------------------------------------------------------------------

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

  let appendoIII = mkToHsText appendoProgram ["x", "y", "xy"]
  putStrLn appendoIII
  appendFile appendoFileName appendoIII

  let appendoOOO = mkToHsText appendoProgram []
  putStrLn appendoOOO
  appendFile appendoFileName appendoOOO

-----------------------------------------------------------------------------------------------------

