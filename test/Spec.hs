module Main (
      main
    ) where

import           Test.Hspec

import           Lib.Peano
import           Res

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
      it "OUT IN OUT" $ do
        take 5 (appendoOIO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[0,1,2,3]),([0,0],[0,0,1,2,3]),([0,0,0],[0,0,0,1,2,3]),([0,0,0,0],[0,0,0,0,1,2,3])]
      it "OUT IN IN" $ do
        appendoOII ([4, 5] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[1..3]]
      it "IN OUT IN" $ do
        appendoIOI ([1..3] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[4, 5]]
-----------------------------------------------------------------------------------------------------
    describe "appendoPat" $ do
      it "IN IN OUT" $ do
        appendoPatIIO ([[1..3], [4, 5]] :: [[Int]]) [[4..5], [6..8]] `shouldBe` [[[1,2,3],[4,5],[4,5],[6,7,8]]]
      it "IN OUT OUT" $ do
        take 5 (appendoPatIOO ([[1..3], [4, 5]] :: [[Int]])) `shouldBe` [([],[[1,2,3],[4,5]]),([[]],[[1,2,3],[4,5],[]]),([[0]],[[1,2,3],[4,5],[0]]),([[1]],[[1,2,3],[4,5],[1]]),([[2]],[[1,2,3],[4,5],[2]])]
      it "IN OUT IN" $ do
        appendoPatIOI ([[1..3], [4, 5]] :: [[Int]]) [[1..3],[4,5],[4,5],[6..8]] `shouldBe` [[[4,5],[6,7,8]]]
-----------------------------------------------------------------------------------------------------
    describe "appendoCtorsUnif" $ do
      it "IN IN OUT" $ do
        appendoCtorsUnifIIO ([[1..3], [4, 5]] :: [[Int]]) [[4..5], [6..8]] `shouldBe` [[[1,2,3],[4,5],[4,5],[6,7,8]]]
-----------------------------------------------------------------------------------------------------
    describe "appendoAssign" $ do
      it "IN IN OUT" $ do
        appendoAssignIIO ([1, 1] :: [Int]) [1..3] `shouldBe` [[1, 1, 1, 2, 3]]
      it "OUT OUT IN" $ do
        appendoAssignOOI ([1, 1, 1, 2, 3] :: [Int]) `shouldBe` [([],[1,1,1,2,3]),([1],[1,1,2,3]),([1,1],[1,2,3])]
      it "IN OUT OUT" $ do
        True `shouldBe` False
--        take 5 (appendoAssignIOO ([1, 1] :: [Int])) `shouldBe` []
      it "OUT IN OUT" $ do
        True `shouldBe` False
--        take 5 (appendoAssignOIO ([1..3] :: [Int])) `shouldBe` []
      it "OUT IN IN" $ do
        appendoAssignOII ([1..3] :: [Int]) ([1, 1, 1, 2, 3] :: [Int]) `shouldBe` [[1, 1]]
      it "IN OUT IN" $ do
        appendoAssignIOI ([1, 1] :: [Int]) ([1, 1, 1, 2, 3] :: [Int]) `shouldBe` [[1..3]]
-----------------------------------------------------------------------------------------------------
    describe "doubleAppendo" $ do
      it "IN OUT" $ do
        doubleAppendoIO ([1..3] :: [Int]) `shouldBe` [[1, 2, 3, 1, 2, 3]]
      it "OUT IN" $ do
        doubleAppendoOI ([1, 2, 3, 1, 2, 3] :: [Int]) `shouldBe` [[1,2,3]]
-----------------------------------------------------------------------------------------------------
    describe "lengtho" $ do
      it "IN OUT" $ do
        lengthoIO ([1..5] :: [Int]) `shouldBe` [5]
      it "OUT IN | need gen type: s2 <- (gen :: [Int])" $ do
        True `shouldBe` True
{-
        lengthoOI (i2p 3) `shouldBe` [[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2],[0,2,0],[0,2,1],[0,2,2],[1,0,0],[1,0,1],[1,0,2],[1,1,0],[1,1,1],[1,1,2],[1,2,0],[1,2,1],[1,2,2],[2,0,0],[2,0,1],[2,0,2],[2,1,0],[2,1,1],[2,1,2],[2,2,0],[2,2,1],[2,2,2]]
-}
-----------------------------------------------------------------------------------------------------
    describe "reverso: reversoRev checks the same but more complicated example" $ do
      it "IN OUT" $ do
        True `shouldBe` True
--        reversoIO ([1, 2, 3] :: [Int]) `shouldBe` [[3, 2, 1]]
      it "OUT IN" $ do
        True `shouldBe` True
--        reversoOI ([3, 2, 1] :: [Int]) `shouldBe` [[1, 2, 3]]
-----------------------------------------------------------------------------------------------------
    describe "reversoRev" $ do
      it "IN OUT" $ do
        reversoRevIO ([1, 2, 3] :: [Int]) `shouldBe` [[3, 2, 1]]
      it "OUT IN" $ do
        reversoRevOI ([3, 2, 1] :: [Int]) `shouldBe` [[1, 2, 3]]
-----------------------------------------------------------------------------------------------------
    describe "revacco" $ do
{-
      it "IN OUT OUT | need gen type: s1 <- (gen :: [[Int]])" $ do
        take 2 (revaccoIOO ([1, 2] :: [Int])) `shouldBe` [([],[2,1]),([0],[2,1,0])]
      it "OUT IN OUT" $ do
        take 5 (revaccoIOO ([1, 2, 3] :: [Int])) `shouldBe` []
-}
      it "IN IN OUT" $ do
        revaccoIIO ([1, 2, 3] :: [Int]) ([0, -1] :: [Int]) `shouldBe` [[3, 2, 1, 0, -1]]
      it "OUT OUT IN" $ do
        take 5 (revaccoOOI ([3, 2, 1, 0, -1] :: [Int])) `shouldBe` [([],[3,2,1,0,-1]),([3],[2,1,0,-1]),([2,3],[1,0,-1]),([1,2,3],[0,-1]),([0,1,2,3],[-1])]
-----------------------------------------------------------------------------------------------------

