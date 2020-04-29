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

  putStrLn "=====================================================================================\n\n"
  let appendoProgram = createProgram nameToDef "appendo" ["x", "y", "xy"]
  print appendoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let annDefs1 = preTranslate appendoProgram ["x", "y"]
  print annDefs1
  print $ translate annDefs1
  putStrLn "-------------------------------------------------------------------------------------\n\n"
{-
  let annDefs2 = preTranslate appendoProgram ["xy"]
  print annDefs2
  print $ translate annDefs2
  putStrLn "-------------------------------------------------------------------------------------\n\n"
-}
{-
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  let appendoGenProgram = createProgram nameToDef "appendoGen" ["x", "y", "xy"]
  let goalStack3 = preTranslate appendoGenProgram [("y", In)]
  print goalStack3
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
  let reversoUnifProgram = createProgram nameToDef "reversoUnif" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoUnifProgram [("x", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoUnifProgram [("y", In)]
  putStrLn "=====================================================================================\n\n"
-}












{-
  putStrLn "=====================================================================================\n\n"
  let reversoProgram = createProgram nameToDef "reverso" ["x", "y"]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print reversoProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoProgram [("x", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ preTranslate reversoProgram [("y", In)]
  putStrLn "=====================================================================================\n\n"
-}




{-
  putStrLn "=====================================================================================\n\n"
  let appendoCtorsUnifProgram = createProgram nameToDef "appendoCtorsUnif" ["x", "y", "xy"]
  print appendoCtorsUnifProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate appendoCtorsUnifProgram [("x", In), ("y", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
--  print $ translate appendoCtorsUnifProgram [("xy", In)]
  putStrLn "=====================================================================================\n\n"
-}




{-
  putStrLn "=====================================================================================\n\n"
  let reversoRevProgram = createProgram nameToDef "reversoRev" ["x", "y"]
  print reversoRevProgram
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate reversoRevProgram [("x", In)]
  putStrLn "-------------------------------------------------------------------------------------\n\n"
  print $ translate reversoRevProgram [("y", In)]
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
