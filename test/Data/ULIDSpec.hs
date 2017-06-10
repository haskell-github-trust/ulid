module Data.ULIDSpec where

import           Control.Concurrent
import           Data.Char
import           Data.List

import           Data.ULID

import           Test.Hspec


spec :: Spec
spec = do
  describe "ulid capabilities" $ do
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
