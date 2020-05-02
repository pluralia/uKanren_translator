module Main (
      main
    ) where

import           Program.Prop
import           Syntax

import           Annotator.Main       (preTranslate)
import           Init
import           Translator           (translate)

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let inDirName = "resources/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs", "extra"]
  nameToDef <- initDefsByNames inFileNames

  -- simple case
{-
  putStrLn "=====================================================================================\n\n"
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]
  print appendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate appendoProgram ["x", "y"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate appendoProgram ["xy"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"
-}

  -- some unifications of IN var
{-
  putStrLn "=====================================================================================\n\n"
  let appendoPatProgram = createProgram nameToDef "appendoPat" ["x", "y", "xy"]
  print appendoPatProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate appendoPatProgram ["x", "y"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate appendoPatProgram ["xy"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"
-}

  -- some invokes; give the same translation independently conjs order
{-
  putStrLn "=====================================================================================\n\n"
  let reversoUnifProgram = createProgram nameToDef "reversoUnif" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate reversoUnifProgram ["x"]
  print annDefs1
--  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
{-
  let annDefs2 = preTranslate reversoUnifProgram ["y"]
  print annDefs2
--  doesn't work because of additional unification
--  print $ translate annDefs2
-}
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let reversoUnifRevProgram = createProgram nameToDef "reversoUnifRev" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoUnifRevProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate reversoUnifRevProgram ["x"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
{-
  let annDefs2 = preTranslate reversoUnifRevProgram ["y"]
  print annDefs2
--  doesn't work because of additional unification
--  print $ translate annDefs2
-}
  putStrLn "=====================================================================================\n\n"
-}

  -- ctor-ctor unification
{-
  putStrLn "=====================================================================================\n\n"
  let appendoCtorsUnifProgram = createProgram nameToDef "appendoCtorsUnif" ["x", "y", "xy"]
  print appendoCtorsUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate appendoCtorsUnifProgram ["x", "y"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate appendoCtorsUnifProgram ["xy"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"
-}

  -- duplicate vars in pattern matching
{-
  putStrLn "=====================================================================================\n\n"
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]
  print appendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs = preTranslate appendoProgram ["x", "xy"]
  print annDefs
  print $ translate annDefs
  putStrLn "=====================================================================================\n\n"
-}

  -- duplicate vars in assigns
{-
  putStrLn "=====================================================================================\n\n"
  let appendoAssignProgram = createProgram nameToDef "appendoAssign" ["x", "y", "xy"]
  print appendoAssignProgram
  let annDefs = preTranslate appendoAssignProgram ["xy"]
  print annDefs
  print $ translate annDefs
  putStrLn "=====================================================================================\n\n"
-}

  -- non-recursive invokes with ctors
{-
  putStrLn "=====================================================================================\n\n"
  let reversoProgram = createProgram nameToDef "reverso" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate reversoProgram ["x"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate reversoProgram ["y"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let zeroAppendoProgram = createProgram nameToDef "zeroAppendo" ["x", "y", "xy"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print zeroAppendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate zeroAppendoProgram ["xy"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate zeroAppendoProgram ["x", "y"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"
-}

  putStrLn "=====================================================================================\n\n"
  let doubleZeroAppendoProgram = createProgram nameToDef "doubleZeroAppendo" ["x", "y", "xy"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print doubleZeroAppendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate doubleZeroAppendoProgram ["x", "y"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs2 = preTranslate doubleZeroAppendoProgram ["xy"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "=====================================================================================\n\n"

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

  -- recursive invokes with ctors
{-
  putStrLn "=====================================================================================\n\n"
  let maxLengthoProgram = createProgram nameToDef "maxLengtho" ["x", "m", "l"]
  print maxLengthoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate maxLengthoProgram ["x", "m"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
{-
  let annDefs2 = preTranslate maxLengthoProgram ["l"]
  print annDefs2
  print $ translate annDefs2
-}
  putStrLn "=====================================================================================\n\n"
-}



{-
  -- simple predicate case: one invoke
  putStrLn "=====================================================================================\n\n"
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]
  print appendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs = preTranslate appendoProgram ["x", "y", "xy"]
  print annDefs
--  print $ translate annDefs
  putStrLn "=====================================================================================\n\n"
-}









{-
  -- check gen
  putStrLn "=====================================================================================\n\n"
  let appendoGenProgram = createProgram nameToDef "appendoGen" ["x", "y", "xy"]
  let annDefs = preTranslate appendoGenProgram ["y"]
  print annDefs
  print $ translate annDefs
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let lengthoProgram = createProgram nameToDef "lengtho" ["x", "l"]
  print lengthoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate lengthoProgram ["x"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  doesn't work because lack of generation
{-
  let annDefs2 = preTranslate lengthoProgram ["l"]
  print annDefs2
  print $ translate annDefs2
-}
  putStrLn "=====================================================================================\n\n"
-}
















{-
  putStrLn "=====================================================================================\n\n"
  let revaccoGenProgram = createProgram nameToDef "revaccoGenIOO" ["xs", "acc", "sx"]
  print revaccoGenProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("xs", In)]
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let revaccoGenProgram = createProgram nameToDef "revaccoGenOIO" ["xs", "acc", "sx"]
  print revaccoGenProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("acc", In)]
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let revaccoGenProgram = createProgram nameToDef "revaccoGenOOIIIOIOIIII" ["xs", "acc", "sx"]
  print revaccoGenProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("sx", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("xs", In), ("acc", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("xs", In), ("sx", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("xs", In), ("acc", In), ("sx", In)]
  putStrLn "=====================================================================================\n\n"
-}
{-
  putStrLn "=====================================================================================\n\n"
  let revaccoGenProgram = createProgram nameToDef "revaccoGenIII" ["xs", "acc", "sx"]
  print revaccoGenProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate revaccoGenProgram [("xs", In), ("acc", In), ("sx", In)]
  putStrLn "=====================================================================================\n\n"
-}








{-
  putStrLn "=====================================================================================\n\n"
  let revaccoProgram = createProgram nameToDef "revacco" ["xs", "acc", "sx"]
  print revaccoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate revaccoProgram [("xs", In), ("acc", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate revaccoProgram [("sx", In)]
  putStrLn "=====================================================================================\n\n"
-}



{-
  putStrLn "=====================================================================================\n\n"
  let plainEvaloProgram = plainQuery'
  print plainEvaloProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate plainEvaloProgram [("res", In)]
  putStrLn "=====================================================================================\n\n"
-}

{-
  putStrLn "=====================================================================================\n\n"
  let queryProgram = Program evalo $ fresh ["st", "fm", "res"] (call "evalo" [V "st", V "fm", V "res"])
  print queryProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate queryProgram [("res", In)]
  putStrLn "=====================================================================================\n\n"
-}
-----------------------------------------------------------------------------------------------------
