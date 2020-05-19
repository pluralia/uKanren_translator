module Main (
      main
    ) where

import           Program.Prop
import           Syntax

import           Init


-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let inDirName   = "resources/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs", "extra", "permSort"]
      outFileName = "test/Res.hs"
  nameToDef <- initDefsByNames inFileNames
  let moduleName = "module Res where"
  let imports = ["\n", "import Lib.Peano", "import Lib.Generator", "\n"]
  writeFile outFileName . unlines $ moduleName : imports

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let minmaxoProgram = createProgram nameToDef "minmaxo" ["a", "b", "min", "max"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  let minmaxoIIOO = mkToHsText minmaxoProgram ["a", "b"]
  putStrLn minmaxoIIOO
  appendFile outFileName minmaxoIIOO

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
{-
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let leoProgram = createProgram nameToDef "leo" ["x", "y", "b"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  let leoIIO = mkToHsText leoProgram ["x", "y"]
  putStrLn leoIIO
  appendFile outFileName leoIIO

  let leoOOI = mkToHsText leoProgram ["b"]
  putStrLn leoOOI
  appendFile outFileName leoOOI

  let leoIOI = mkToHsText leoProgram ["x", "b"]
  putStrLn leoIOI
  appendFile outFileName leoIOI

  let leoOII = mkToHsText leoProgram ["y", "b"]
  putStrLn leoOII
  appendFile outFileName leoOII

  let leoIOO = mkToHsText leoProgram ["x"]
  putStrLn leoIOO
  appendFile outFileName leoIOO

  let leoOIO = mkToHsText leoProgram ["y"]
  putStrLn leoOIO
  appendFile outFileName leoOIO

  let leoIII = mkToHsText leoProgram ["x", "y", "b"]
  putStrLn leoIII
  appendFile outFileName leoIII

  let leoOOO = mkToHsText leoProgram []
  putStrLn leoOOO
  appendFile outFileName leoOOO
-}
-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

{-
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let permSortProgram = createProgram nameToDef "permSort" ["l", "sorted"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  let permSortIO = mkToHsText permSortProgram ["l"]
  putStrLn permSortIO
  appendFile outFileName permSortIO
-}
-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

