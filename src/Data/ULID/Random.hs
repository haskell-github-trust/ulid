-- | Helper functions to generate the random part of an ULID
-- either with PRNGs or TRNGs.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.ULID.Random (
    ULIDRandom,
    mkCryptoULIDRandom,
    mkULIDRandom,
    getULIDRandom
) where

import Control.DeepSeq
import Control.Monad
import Crypto.Random
import Data.Binary
import Data.Binary.Roll
import Data.ByteString as BS hiding (split, take)
import Data.Data
import Data.Maybe
import Data.Word
import Data.Text as T hiding (split, take)
import GHC.Generics
import System.Random


import qualified Data.ULID.Base32 as B32


-- | Newtype wrapping a `ByteString`
newtype ULIDRandom = ULIDRandom BS.ByteString
    deriving (Eq, Typeable, Data, Generic)

instance Show ULIDRandom where
    show (ULIDRandom r) = T.unpack $ B32.encode 16.roll.(BS.unpack) $ r

instance Read ULIDRandom where
  readsPrec _ = fmap
    (\(int, rest) ->
        (ULIDRandom $ BS.pack $ unroll numBytes int, T.unpack rest))
    . (B32.decode $ 16)
    . T.pack

instance Binary ULIDRandom where
    put (ULIDRandom r) = mapM_ put (BS.unpack $ r)
    get = ULIDRandom <$> (BS.pack) <$> replicateM numBytes get

instance NFData ULIDRandom where
    rnf (ULIDRandom r) = rnf r


numBytes = 10 -- 80 bits


-- | Generate a `ULIDRandom` based on a cryptographically secure
-- random number generator.
-- See:
-- https://hackage.haskell.org/package/crypto-api-0.13.3/docs/Crypto-Random.html
mkCryptoULIDRandom :: CryptoRandomGen g => g -> Either GenError (ULIDRandom, g)
mkCryptoULIDRandom g = do
  (b, g2) <- genBytes numBytes g
  return (ULIDRandom b, g2)


-- | Generate a `ULIDRandom` based on a standard random number generator.
-- See:
-- https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
mkULIDRandom :: RandomGen g => g -> (ULIDRandom, g)
mkULIDRandom g = let
  (g1, g2) = split g
  genbytes = (BS.pack) . take numBytes . randoms
  in (ULIDRandom $ genbytes g, g2)


-- | Generate a ULID Random based on the global random number generator.
getULIDRandom :: IO ULIDRandom
-- | Note: The call to `newStdGen` splits the generator,
-- so this is safe to call multiple times
getULIDRandom =
  fst <$> mkULIDRandom <$> newStdGen
