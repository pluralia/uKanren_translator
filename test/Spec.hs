module Main (
      main
    ) where

import           Data.List (sort)
import           Test.Hspec

import           Lib.Peano
import           Appendo
import           AppendoCtorUnif
import           DoubleAppendo
import           Reverso
import           Revacco
import           PermSort

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  hspec $ do
-----------------------------------------------------------------------------------------------------

    describe "permSort" $ do
      it "IN OUT" $ do
        let l = [23, 8, 3, 6, 76, 4, 63, 65, 23, 65, 77, 3]
        (fmap (fmap p2i) . permSortIO . fmap i2p $ l) `shouldBe` [sort l]
      it "OUT IN" $ do
        length (permSortOI ([1..9])) `shouldBe` 362880

-----------------------------------------------------------------------------------------------------

    describe "revacco" $ do
      it "IN OUT OUT" $ do
        take 5 (revaccoIOO ([1, 2] :: [Int])) `shouldBe` [([],[2,1]),([0],[2,1,0]),([1],[2,1,1]),([2],[2,1,2]),([3],[2,1,3])]
      it "OUT IN IN" $ do
        take 1 (revaccoOII ([4, 5] :: [Int]) ([3, 2, 1, 4, 5] :: [Int])) `shouldBe` [[1..3]]
      it "OUT IN OUT" $ do
        take 5 (revaccoOIO ([1, 2] :: [Int])) `shouldBe` [([],[1,2]),([0],[2,1,0]),([0,0],[2,1,0,0]),([0,1],[2,1,0,1]),([0,2],[2,1,0,2])]
      it "IN OUT IN" $ do
        revaccoIOI ([1..3] :: [Int]) ([3, 2, 1, 4, 5] :: [Int]) `shouldBe` [[4, 5]]
      it "OUT OUT IN" $ do
        take 4 (revaccoOOI ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([1],[2,3]),([2,1],[3]),([3,2,1],[])]
      it "IN IN OUT" $ do
        revaccoIIO ([1..3] :: [Int]) ([0, -1] :: [Int]) `shouldBe` [[3, 2, 1, 0, -1]]
      it "IN IN IN: True" $ do
        take 1 (revaccoIII ([1,2] :: [Int]) ([1,5] :: [Int]) ([2,1,1,5] :: [Int])) `shouldBe` [()]
      it "IN IN IN: False" $ do
        take 1 (revaccoIII ([1,2] :: [Int]) ([1,5] :: [Int]) ([2,1,5] :: [Int])) `shouldBe` []
      it "OUT OUT OUT" $ do
        take 5 revaccoOOO `shouldBe` [([],[],[]),([],[0],[0]),([],[1],[1]),([],[2],[2]),([],[3],[3])]

-----------------------------------------------------------------------------------------------------

    describe "reverso" $ do
      it "IN OUT" $ do
        reversoIO ([1, 2, 3] :: [Int]) `shouldBe` [[3, 2, 1]]
      it "OUT IN" $ do
        reversoOI ([3, 2, 1] :: [Int]) `shouldBe` [[1, 2, 3]]
      it "IN IN: True" $ do
        take 1 (reversoII ([3, 2, 1] :: [Int]) ([1, 2, 3] :: [Int])) `shouldBe` [()]
      it "IN IN: False" $ do
        take 1 (reversoII ([3, 2, 1] :: [Int]) ([1] :: [Int])) `shouldBe` []
      it "OUT OUT" $ do
        take 5 reversoOO `shouldBe` [([],[]),([0],[0]),([0,0],[0,0]),([0,1],[1,0]),([0,2],[2,0])]

-----------------------------------------------------------------------------------------------------

    describe "doubleAppendo" $ do
      it "IN OUT" $ do
        doubleAppendoIO ([1..3] :: [Int]) `shouldBe` [[1, 2, 3, 1, 2, 3]]
      it "OUT IN" $ do
        doubleAppendoOI ([1, 2, 3, 1, 2, 3] :: [Int]) `shouldBe` [[1,2,3]]
      it "IN IN: True" $ do
        take 1 (doubleAppendoII ([1..3] :: [Int]) ([1, 2, 3, 1, 2, 3] :: [Int])) `shouldBe` [()]
      it "IN IN: False" $ do
        take 1 (doubleAppendoII ([1..3] :: [Int]) ([1] :: [Int])) `shouldBe` []
      it "OUT OUT | INF" $ do
        take 1 doubleAppendoOO `shouldBe` [([],[])]

-----------------------------------------------------------------------------------------------------

    describe "appendoCtorUnif" $ do
      it "IN IN OUT" $ do
        appendoCtorUnifIIO ([1..3] :: [Int]) [4..5] `shouldBe` [[1..5]]
      it "OUT OUT IN" $ do
        appendoCtorUnifOOI ([1..3] :: [Int]) `shouldBe` [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
      it "IN OUT OUT" $ do
        take 5 (appendoIOO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[1,2,3,0]),([1],[1,2,3,1]),([2],[1,2,3,2]),([3],[1,2,3,3])]
      it "OUT IN IN" $ do
        appendoCtorUnifOII ([4, 5] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[1..3]]
      it "OUT IN OUT" $ do
        take 5 (appendoCtorUnifOIO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[0,1,2,3]),([0,0],[0,0,1,2,3]),([0,0,0],[0,0,0,1,2,3]),([0,0,0,0],[0,0,0,0,1,2,3])]
      it "IN OUT IN" $ do
        appendoCtorUnifIOI ([1..3] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[4, 5]]
      it "IN IN IN: True" $ do
        take 1 (appendoCtorUnifIII ([1..3] :: [Int]) ([1,5] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` [()]
      it "IN IN IN: False" $ do
        take 1 (appendoCtorUnifIII ([1..3] :: [Int]) ([1] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` []
      it "OUT OUT OUT" $ do
        take 5 appendoCtorUnifOOO `shouldBe` [([],[],[]),([],[0],[0]),([],[1],[1]),([],[2],[2]),([],[3],[3])]

-----------------------------------------------------------------------------------------------------

    describe "appendo" $ do
      it "IN IN OUT" $ do
        appendoIIO ([1..3] :: [Int]) [4..5] `shouldBe` [[1..5]]
      it "OUT OUT IN" $ do
        appendoOOI ([1..3] :: [Int]) `shouldBe` [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
      it "IN OUT OUT" $ do
        take 5 (appendoIOO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[1,2,3,0]),([1],[1,2,3,1]),([2],[1,2,3,2]),([3],[1,2,3,3])]
      it "OUT IN IN" $ do
        appendoOII ([4, 5] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[1..3]]
      it "OUT IN OUT" $ do
        take 5 (appendoOIO ([1..3] :: [Int])) `shouldBe` [([],[1,2,3]),([0],[0,1,2,3]),([0,0],[0,0,1,2,3]),([0,0,0],[0,0,0,1,2,3]),([0,0,0,0],[0,0,0,0,1,2,3])]
      it "IN OUT IN" $ do
        appendoIOI ([1..3] :: [Int]) ([1..5] :: [Int]) `shouldBe` [[4, 5]]
      it "IN IN IN: True" $ do
        take 1 (appendoIII ([1..3] :: [Int]) ([1,5] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` [()]
      it "IN IN IN: False" $ do
        take 1 (appendoIII ([1..3] :: [Int]) ([1] :: [Int]) ([1, 2, 3, 1, 5] :: [Int])) `shouldBe` []
      it "OUT OUT OUT" $ do
        take 5 appendoOOO `shouldBe` [([],[],[]),([],[0],[0]),([],[1],[1]),([],[2],[2]),([],[3],[3])]

-----------------------------------------------------------------------------------------------------

