module Main (
      main
    ) where

import           Test.Hspec

import           Lib.Peano
import           Appendo

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  hspec $ do

-----------------------------------------------------------------------------------------------------

    describe "appendo" $ do
      it "IN IN OUT" $ do
        appendoIIO ([1..3] :: [Int]) [4..5] `shouldBe` [[1..5]]
      it "OUT OUT IN" $ do
        appendoOOI ([1..3] :: [Int]) `shouldBe` [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
      it "IN OUT OUT" $ do
        take 5 (appendoIOO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[1,2,3,0]),([1],[1,2,3,1]),([2],[1,2,3,2]),([0,0],[1,2,3,0,0])]
      it "OUT IN IN" $ do
        appendoOII ([4, 5] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[1..3]]
      it "OUT IN OUT" $ do
        take 5 (appendoOIO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[0,1,2,3]),([0,0],[0,0,1,2,3]),([0,0,0],[0,0,0,1,2,3]),([0,0,0,0],[0,0,0,0,1,2,3])]
      it "IN OUT IN" $ do
        appendoIOI ([1..3] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[4, 5]]

{-
      it "IN IN IN: True" $ do
        take 1 (appendoIII ([1..3] :: [Int]) ([1,5] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` [()]
      it "IN IN IN: False" $ do
        take 1 (appendoIII ([1..3] :: [Int]) ([1] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` []
-}

-----------------------------------------------------------------------------------------------------

