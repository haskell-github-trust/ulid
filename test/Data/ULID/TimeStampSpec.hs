module Data.ULID.TimeStampSpec where

import           Data.ULID.TimeStamp

import           Test.Hspec


spec :: Spec
spec = do
  describe "show/read" $ do
        it "works with a known value" $ do
            show (mkULIDTimeStamp 1469918176.385) `shouldBe` "01ARYZ6S41"
        it "has show/read symmetry" $ do
            let a1 = mkULIDTimeStamp 12345
            let a2 = mkULIDTimeStamp 54321
            a1 == a2 `shouldBe` False
            read (show a1) `shouldBe` a1
            read (show a2) `shouldBe` a2
