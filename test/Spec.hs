import Test.Hspec

countSmth :: String -> IO String
countSmth = return


main :: IO ()
main = hspec $ do
-----------------------------------------------------------------------------------------------------
  describe "appendo" $ do
    it "app" $ do
      ast <- countSmth "app"
      ast `shouldBe` "app"
