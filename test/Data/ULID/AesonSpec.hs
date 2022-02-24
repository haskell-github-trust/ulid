{-# LANGUAGE TypeApplications #-}
module Data.ULID.AesonSpec where

import Data.ULID
import Data.ULID.Random
import Data.ByteString.Lazy
import qualified Data.Aeson as Aeson
import Data.Text.Lazy.Encoding
import Data.Text.Lazy as T
import Data.ULID.Aeson ()

import Test.Hspec
import Data.String (IsString(fromString))


spec :: Spec
spec = do
  describe "ULID fromJson/toJson" $ do
    it "Same after encode -> decode" $ do
        ulid <- getULID
        Aeson.eitherDecode (Aeson.encode ulid) `shouldBe` Right ulid
        read (show ulid) `shouldBe` ulid

    it "decodes similar to read" $ do
        ulid_str <- show <$> getULID
        let
            ulid_bs :: ByteString
            ulid_bs = encodeUtf8 $ T.pack ("\"" <> ulid_str <> "\"")
        Aeson.eitherDecode ulid_bs `shouldBe` Right (read @ULID ulid_str)

  describe "ULIDRandom fromJson/toJson" $ do
    it "Same after encode -> decode" $ do
        ulid <- getULIDRandom
        Aeson.eitherDecode (Aeson.encode ulid) `shouldBe` Right ulid
        read (show ulid) `shouldBe` ulid

    it "decodes similar to read" $ do
        ulid_str <- show <$> getULIDRandom
        let
            ulid_bs :: ByteString
            ulid_bs = encodeUtf8 $ T.pack ("\"" <> ulid_str <> "\"")
        Aeson.eitherDecode ulid_bs `shouldBe` Right (read @ULIDRandom ulid_str)
