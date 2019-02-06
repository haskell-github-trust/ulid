module Data.ULIDSpec where

import           Control.Concurrent
import           Control.Monad        (replicateM)
import           Data.Binary
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Hashable
import           Data.List            (nub, sort)
import qualified System.Random        as R

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
            encode a1 `shouldNotBe` encode a2
        it "encodes MSB first" $ do
            a1 <- getULIDTime 12345
            let e1 = encode a1
            -- this works because the time value is small, and time is sequences first, so the MSB for this value should be 0
            LBS.head e1 `shouldBe` 0
            LBS.last e1 `shouldNotBe` 0
  describe "random" $ do
        it "works in IO" $ do
            u1 <- (R.randomIO :: IO ULID)
            u2 <- (R.randomIO :: IO ULID)
            u1 `shouldNotBe` u2
        it "works with randomgen" $ do
            g <- R.getStdGen
            let (u1, g') = R.random g :: (ULID, R.StdGen)
            let (u2, _) = R.random g' :: (ULID, R.StdGen)
            u1 `shouldNotBe` u2
  describe "hash" $ do
        -- The general contract of hashWithSalt is:
        -- If two values are equal according to the == method,
        -- then applying the hashWithSalt method on each of the two values
        -- must produce the same integer result if the same salt is used in each case.
        it "produces same hash for equal ulids" $ do
            u1 <- getULID
            let u2 = (read (show u1)) :: ULID
            let salt = 12345
            hashWithSalt salt u1 `shouldBe` hashWithSalt salt u2
        -- It is not required that if two values are unequal according to the == method,
        -- then applying the hashWithSalt method on each of the two values must produce distinct integer results.
        -- However, the programmer should be aware that producing distinct integer results for unequal values
        -- may improve the performance of hashing-based data structures.
        it "produces different hash for nonequals ulids" $ do
            u1 <- getULID
            u2 <- getULID
            let salt = 12345
            -- this could rarely fail due to hash nature
            hashWithSalt salt u1 `shouldNotBe` hashWithSalt salt u2
        -- This method can be used to compute different hash values for the same input by
        -- providing a different salt in each application of the method.
        -- This implies that any instance that defines hashWithSaltmust make use of the salt in its implementation.
        it "produces different hash for equals ulids with different salt" $ do
            u1 <- getULID
            let u2 = (read (show u1)) :: ULID
            let salt = 12345
            hashWithSalt salt u1 `shouldBe` hashWithSalt salt u2
            let salt2 = 54321
            hashWithSalt salt u1 `shouldNotBe` hashWithSalt salt2 u2
  describe "to/from integer" $ do
      it "is sortable" $ do
            u1 <- getULID
            threadDelay 1000
            u2 <- getULID
            threadDelay 1000
            u3 <- getULID
            threadDelay 1000
            u4 <- getULID
            threadDelay 1000
            let l = [ulidToInteger u3, ulidToInteger u2, ulidToInteger u4, ulidToInteger u1]
            let l' = sort l
            l' `shouldBe` [ulidToInteger u1, ulidToInteger u2, ulidToInteger u3, ulidToInteger u4]
      it "has to/from symmetry" $ do
            a1 <- getULID
            a2 <- getULID
            a1 == a2 `shouldBe` False
            ulidFromInteger (ulidToInteger a1) `shouldBe` a1
            ulidFromInteger (ulidToInteger a2) `shouldBe` a2
            ulidToInteger a1 `shouldNotBe` ulidToInteger a2
      it "handles negative integers" $ do
            a1 <- getULID
            ulidFromInteger (negate (ulidToInteger a1)) `shouldBe` a1
