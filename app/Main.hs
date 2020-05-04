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
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs", "extra"]
      outFileName = "test/Res.hs"
  nameToDef <- initDefsByNames inFileNames
  let moduleName = "module Res where"
  let imports = ["\n", "import Lib.Peano", "import Lib.Generator", "\n"]
  writeFile outFileName . unlines $ moduleName : imports

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- GENERAL vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{-
  -- simple case
  let appendoIIO = mkToHsText appendoProgram ["x", "y"]
  putStrLn appendoIIO
  appendFile outFileName appendoIIO

  -- simple case
  let appendoOOI = mkToHsText appendoProgram ["xy"]
  putStrLn appendoOOI
  appendFile outFileName appendoOOI
-}
  -- generation
  let appendoIOO = mkToHsText appendoProgram ["x"]
  putStrLn appendoIOO
  appendFile outFileName appendoIOO

  -- generation
  let appendoOIO = mkToHsText appendoProgram ["y"]
  putStrLn appendoOIO
  appendFile outFileName appendoOIO

  -- duplicate vars in pattern matching
  let appendoOII = mkToHsText appendoProgram ["y", "xy"]
  putStrLn appendoOII
  appendFile outFileName appendoOII

  -- duplicate vars in pattern matching
  let appendoIOI = mkToHsText appendoProgram ["x", "xy"]
  putStrLn appendoIOI
  appendFile outFileName appendoIOI

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- SOME UNIFICATIONS OF IN VARS vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let appendoPatProgram = createProgram nameToDef "appendoPat" ["x", "y", "xy"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- simple case
  let appendoPatIIO = mkToHsText appendoPatProgram ["x", "y"]
  putStrLn appendoPatIIO
  appendFile outFileName appendoPatIIO

  -- generation
  let appendoPatIOO = mkToHsText appendoPatProgram ["x"]
  putStrLn appendoPatIOO
  appendFile outFileName appendoPatIOO

  -- duplicate vars in pattern matching
  let appendoPatIOI = mkToHsText appendoPatProgram ["x", "xy"]
  putStrLn appendoPatIOI
  appendFile outFileName appendoPatIOI

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- CTOR-CTOR UNIFICATION vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let appendoCtorsUnifProgram = createProgram nameToDef "appendoCtorsUnif" ["x", "y", "xy"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- simple case
  let appendoCtorsUnifIIO = mkToHsText appendoCtorsUnifProgram ["x", "y"]
  putStrLn appendoCtorsUnifIIO
  appendFile outFileName appendoCtorsUnifIIO

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- DUPLICATE VARS IN ASSIGNS vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let appendoAssignProgram = createProgram nameToDef "appendoAssign" ["x", "y", "xy"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- simple case
  let appendoAssignIIO = mkToHsText appendoAssignProgram ["x", "y"]
  putStrLn appendoAssignIIO
  appendFile outFileName appendoAssignIIO

  -- duplicate vars in pattern matching
  let appendoAssignOOI = mkToHsText appendoAssignProgram ["xy"]
  putStrLn appendoAssignOOI
  appendFile outFileName appendoAssignOOI
{-
  -- 
  let appendoAssignIOO = mkToHsText appendoAssignProgram ["x"]
  putStrLn appendoAssignIOO
  appendFile outFileName appendoAssignIOO

  -- 
  let appendoAssignOIO = mkToHsText appendoAssignProgram ["y"]
  putStrLn appendoAssignOIO
  appendFile outFileName appendoAssignOIO
-}
  -- duplicate vars in pattern matching
  let appendoAssignOII = mkToHsText appendoAssignProgram ["y", "xy"]
  putStrLn appendoAssignOII
  appendFile outFileName appendoAssignOII

  -- duplicate vars in pattern matching
  let appendoAssignIOI = mkToHsText appendoAssignProgram ["x", "xy"]
  putStrLn appendoAssignIOI
  appendFile outFileName appendoAssignIOI

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- DUPLICATE IN VARS IN INVOKES vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let doubleAppendoProgram = createProgram nameToDef "doubleAppendo" ["x", "xy"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- simple case
  let doubleAppendoIO = mkToHsText doubleAppendoProgram ["x"]
  putStrLn doubleAppendoIO
  appendFile outFileName doubleAppendoIO

  -- duplicate vars in assign
  let doubleAppendoOI = mkToHsText doubleAppendoProgram ["xy"]
  putStrLn doubleAppendoOI
  appendFile outFileName doubleAppendoOI

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- GENERAL vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let lengthoProgram = createProgram nameToDef "lengtho" ["x", "l"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- simple case
  let lengthoIO = mkToHsText lengthoProgram ["x"]
  putStrLn lengthoIO
  appendFile outFileName lengthoIO

  -- generation
  let lengthoOI = mkToHsText lengthoProgram ["l"]
  putStrLn lengthoOI
  appendFile outFileName lengthoOI

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
{-
-- NON-RECURSIVE INVOKE WITH CTOR vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let reversoProgram = createProgram nameToDef "reverso" ["x", "y"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- some invokes
  let reversoIO = mkToHsText reversoProgram ["x"]
  putStrLn reversoIO
  appendFile outFileName reversoIO

  -- some invokes
  let reversoIO = mkToHsText reversoProgram ["y"]
  putStrLn reversoIO
  appendFile outFileName reversoIO

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"
-}
-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- NON-RECURSIVE INVOKE WITH CTOR && SOME INVOKES IN "WRONG" ORDER vvvvvv
  let reversoRevProgram = createProgram nameToDef "reversoRev" ["x", "y"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- some invokes
  let reversoRevIO = mkToHsText reversoRevProgram ["x"]
  putStrLn reversoRevIO
  appendFile outFileName reversoRevIO

  -- some invokes
  let reversoRevIO = mkToHsText reversoRevProgram ["y"]
  putStrLn reversoRevIO
  appendFile outFileName reversoRevIO

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

-- RECURSIVE INVOKE WITH CTOR vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  let revaccoProgram = createProgram nameToDef "revacco" ["xs", "acc", "sx"]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- 
  let revaccoIOO = mkToHsText revaccoProgram ["xs"]
  putStrLn revaccoIOO
  appendFile outFileName revaccoIOO

  -- 
  let revaccoOIO = mkToHsText revaccoProgram ["acc"]
  putStrLn revaccoOIO
  appendFile outFileName revaccoOIO

  -- 
  let revaccoOOI = mkToHsText revaccoProgram ["sx"]
  putStrLn revaccoOOI
  appendFile outFileName revaccoOOI

  -- 
  let revaccoIIO = mkToHsText revaccoProgram ["xs", "acc"]
  putStrLn revaccoIIO
  appendFile outFileName revaccoIIO

  -- 
  let revaccoIOI = mkToHsText revaccoProgram ["xs", "sx"]
  putStrLn revaccoIOI
  appendFile outFileName revaccoIOI

  -- 
  let revaccoOII = mkToHsText revaccoProgram ["acc", "sx"]
  putStrLn revaccoOII
  appendFile outFileName revaccoOII

  appendFile outFileName 
    "-------------------------------------------------------------------------------------\n"

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

