module Data.ULID.RandomSpec where

import           Data.Binary
import qualified Data.ByteString.Lazy as LBS

import           Data.ULID.Random

import           Test.Hspec


spec :: Spec
spec = do
    describe "show/read" $ do
        it "has show/read symmetry" $ do
            a1 <- getULIDRandom
            a2 <- getULIDRandom
            a1 == a2 `shouldBe` False
            read (show a1) `shouldBe` a1
            read (show a2) `shouldBe` a2
        it "has correct show length" $ do
            a1 <- getULIDRandom
            length (show a1) `shouldBe` 16
    describe "encode/decode" $ do
        it "has correct binary length" $ do
            a1 <- getULIDRandom
            LBS.length (encode a1) `shouldBe` 10 -- 80 bit
        it "has encode/decode symmetry" $ do
            a1 <- getULIDRandom
            a2 <- getULIDRandom
            a1 == a2 `shouldBe` False
            decode (encode a1) `shouldBe` a1
            decode (encode a2) `shouldBe` a2
