module Data.ULIDSpec where

import           Control.Concurrent
import           Control.Monad        (replicateM)
import           Data.Binary
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.List
import           Data.List            (nub)

import           Data.ULID

import           Test.Hspec


spec :: Spec
spec = do
  describe "ulid capabilities" $ do
        it "binary length 128-bit" $ do
            a1 <- getULID
            LBS.length (encode a1) `shouldBe` 16 -- 128 bit
        it "is lexicographically sortable" $ do
            u1 <- getULID
            threadDelay 1000
            u2 <- getULID
            threadDelay 1000
            u3 <- getULID
            threadDelay 1000
            u4 <- getULID
            threadDelay 1000
            let l = [show u3, show u2, show u4, show u1]
            let l' = sort l
            l' `shouldBe` [show u1, show u2, show u3, show u4]

            -- make sure it works in internal representation too :)
            let ul = [u3, u2, u4, u1]
            let ul' = sort ul
            ul' `shouldBe` [u1, u2, u3, u4]
        it "is encoded as 26 character string" $ do
            u1 <- getULID
            length (show u1) `shouldBe` 26
        it "is case-insensitive" $ do
            u1 <- getULID
            let u2 = read (map toLower (show u1))
            let u3 = read (map toUpper (show u1))
            u1 `shouldBe` u2
            u1 `shouldBe` u3
        it "no special characters" $ do
            u1 <- getULID
            filter (not.isAlphaNum) (show u1) `shouldBe` []
  describe "ulid" $ do
        it "starts with 0 (at least for the foreseeable future)" $ do
            u1 <- getULID
            head (show u1) `shouldBe` '0'
        it "generates unique ulids in default configuration" $ do
            let ops = 1000
            ulids <- replicateM ops getULID
            -- Verify uniqueness
            let n' = length $ nub ulids
            n' `shouldBe` ops
  describe "encode/decode" $ do
        it "has encode/decode symmetry" $ do
            a1 <- getULID
            a2 <- getULID
            a1 == a2 `shouldBe` False
            decode (encode a1) `shouldBe` a1
            decode (encode a2) `shouldBe` a2
        it "encodes MSB first" $ do
            a1 <- getULIDTime 12345
            let e1 = encode a1
            -- this works because the time value is small, and time is sequences first, so the MSB for this value should be 0
            LBS.head e1 `shouldBe` 0
            LBS.last e1 `shouldNotBe` 0
