module Main (
      main
    ) where

import           Test.Hspec

import           Program.Prop
import           Syntax

import           Annotator.Main       (preTranslate)
import           Init
import           Translator           (translate)

-----------------------------------------------------------------------------------------------------

ann = preTranslate
trans program = translate . preTranslate program

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let inDirName = "resources/"
      inFileNames = (inDirName ++) <$> ["list", "num", "bool", "programs", "extra"]
  nameToDef <- initDefsByNames inFileNames
  hspec $ do

-----------------------------------------------------------------------------------------------------

    describe "appendo: ann" $ do
      let program = createProgram nameToDef "appendo" ["x", "y", "xy"]
      it "IN IN OUT" $ do
        show (ann program ["x", "y"]) `shouldBe` "[AnnDef \"appendo\" [(0,0),(1,0),(2,1)] [[U v.(0,0) [],U v.(2,1) v.(1,0)],[U v.(0,0) (v.(3,1) : v.(4,1)),U v.(2,3) (v.(3,1) : v.(5,2)),I \"appendo\" [(4,1),(1,0),(5,2)]]]]"
      it "OUT OUT IN" $ do
        show (ann program ["xy"]) `shouldBe` "[AnnDef \"appendo\" [(0,1),(1,1),(2,0)] [[U v.(0,1) [],U v.(2,0) v.(1,1)],[U v.(0,3) (v.(3,1) : v.(4,2)),U v.(2,0) (v.(3,1) : v.(5,1)),I \"appendo\" [(4,2),(1,2),(5,1)]]]]"

    describe "appendo: trans" $ do
      let program = createProgram nameToDef "appendo" ["x", "y", "xy"]
      it "IN IN OUT" $ do
        show (trans program ["x", "y"]) `shouldBe` "[appendo x0 x1 = appendo0 x0 x1 ++ appendo1 x0 x1\nappendo0 [] s2 = return $ (s2)\nappendo0 _ _ = []\nappendo1 (s3 : s4) s1 = do\n  (s5) <- appendo s4 s1\n  let s2 = (s3 : s5)\n  return $ (s2)\nappendo1 _ _ = []\n]"
      it "OUT OUT IN" $ do
        show (trans program ["xy"]) `shouldBe` "[appendo x0 = appendo0 x0 ++ appendo1 x0\nappendo0 s1 = do\n  let s0 = []\n  return $ (s0, s1)\nappendo0 _ = []\nappendo1 (s3 : s5) = do\n  (s4, s1) <- appendo s5\n  let s0 = (s3 : s4)\n  return $ (s0, s1)\nappendo1 _ = []\n]"

-----------------------------------------------------------------------------------------------------

