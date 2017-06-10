module Data.ULID.CrockfordSpec where

import           Data.ULID.Crockford

import           Test.Hspec


spec :: Spec
spec = do
  describe "encode" $ do
      -- known examples from https://crockfordbase32.codeplex.com/
        it "compare to known examples" $ do
            encode 1 1 `shouldBe` "1"
            encode 2 194 `shouldBe` "62"
            encode 11 3838385658376483 `shouldBe` "3D2ZQ6TVC93"
            encode 13 18446744073709551615 `shouldBe` "FZZZZZZZZZZZZ"
        it "compare to known examples (padded)" $ do
            encode 3 1 `shouldBe` "001"
            encode 3 194 `shouldBe` "062"
            encode 15 3838385658376483 `shouldBe` "00003D2ZQ6TVC93"
  describe "decode" $ do
      -- known examples from https://crockfordbase32.codeplex.com/
        it "compare to known examples" $ do
            decode 1 "1" `shouldBe` [(1, "")]
            decode 2 "62" `shouldBe` [(194, "")]
            decode 11 "3D2ZQ6TVC93" `shouldBe` [(3838385658376483, "")]
            decode 13 "FZZZZZZZZZZZZ" `shouldBe` [(18446744073709551615, "")]
        it "compare to known examples (padded)" $ do
            decode 3 "001" `shouldBe` [(1, "")]
            decode 3 "062" `shouldBe` [(194, "")]
            decode 15 "00003D2ZQ6TVC93" `shouldBe` [(3838385658376483, "")]
        it "gives remainder string" $ do
            decode 3 "001ABC" `shouldBe` [(1, "ABC")]
            decode 3 "062DEF" `shouldBe` [(194, "DEF")]
            decode 15 "00003D2ZQ6TVC93X1" `shouldBe` [(3838385658376483, "X1")]
        it "gives empty list if invalid" $ do
            decode 3 "U01ABC" `shouldBe` []
            decode 2 "!01DEF" `shouldBe` []
