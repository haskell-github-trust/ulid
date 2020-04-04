module Data.ULID.TimeStampSpec where

import           Data.Binary
import qualified Data.ByteString.Lazy as LBS

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

    it "has correct show length" $ do
        let a1 = mkULIDTimeStamp 12345
        length (show a1) `shouldBe` 10

  describe "encode/decode" $ do
    it "has correct binary length" $ do
        let a1 = mkULIDTimeStamp 12345
        LBS.length (encode a1) `shouldBe` 6 -- 48 bit

    it "has encode/decode symmetry" $ do
        let a1 = mkULIDTimeStamp 12345
        let a2 = mkULIDTimeStamp 54321
        a1 == a2 `shouldBe` False
        decode (encode a1) `shouldBe` a1
        decode (encode a2) `shouldBe` a2

    it "encodes MSB first" $ do
        let a1 = mkULIDTimeStamp 12345
        let e1 = encode a1
        -- This works because the value is small,
        -- so the MSB for this value should be 0
        LBS.head e1 `shouldBe` 0
        LBS.last e1 `shouldNotBe` 0
