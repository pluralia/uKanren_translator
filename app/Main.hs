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

{-
  -- simple case
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

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

{-
  -- some invokes
  putStrLn "=====================================================================================\n\n"
  let reversoUnifProgram = createProgram nameToDef "reversoUnif" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoUnifProgram ["x"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ preTranslate reversoUnifProgram ["y"]
  putStrLn "=====================================================================================\n\n"


  putStrLn "=====================================================================================\n\n"
  let reversoUnifRevProgram = createProgram nameToDef "reversoUnifRev" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoUnifRevProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoUnifRevProgram ["x"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ preTranslate reversoUnifRevProgram ["y"]
  putStrLn "=====================================================================================\n\n"
-}

{-
  -- some invokes: check conj independency
  putStrLn "=====================================================================================\n\n"
  let reversoRevProgram = createProgram nameToDef "reversoRev" ["x", "y"]
  print reversoRevProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate reversoRevProgram ["x"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ preTranslate reversoRevProgram ["y"]
  putStrLn "=====================================================================================\n\n"
-}



{-
  -- predicate case
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
  let appendoCtorsUnifProgram = createProgram nameToDef "appendoCtorsUnif" ["x", "y", "xy"]
  print appendoCtorsUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate appendoCtorsUnifProgram ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate appendoCtorsUnifProgram ["xy"]
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
  let reversoProgram = createProgram nameToDef "reverso" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoProgram ["x"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ preTranslate reversoProgram ["y"]
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
  let lengthoProgram = createProgram nameToDef "lengtho" ["x", "l"]
  print lengthoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate lengthoProgram [("x", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate lengthoProgram [("l", In)]
  putStrLn "=====================================================================================\n\n"
-}

{-
  putStrLn "=====================================================================================\n\n"
  let maxLengthoProgram = createProgram nameToDef "maxLengtho" ["x", "m", "l"]
  print maxLengthoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate maxLengthoProgram [("x", In), ("m", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ translate maxLengthoProgram [("l", In)]
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
