module Data.ULID.Random (
    ULIDRandom,
    mkCryptoULIDRandom,
    mkULIDRandom,
    getULIDRandom
) where

import           Control.DeepSeq
import           Control.Monad
import           Crypto.Random
import           Data.Binary
import           Data.Binary.Roll
import qualified Data.ByteString     as BS
import           Data.Word
import           System.Random


import qualified Data.ULID.Crockford as CR


newtype ULIDRandom = ULIDRandom BS.ByteString
    deriving (Eq)

numBytes = 10 -- 80 bits

-- | Generate a ULID Random based on a cryptographically secure random number generator.
-- | see: https://hackage.haskell.org/package/crypto-api-0.13.2/docs/Crypto-Random.html
mkCryptoULIDRandom :: CryptoRandomGen g => g -> Either GenError (ULIDRandom, g)
mkCryptoULIDRandom g = do
    (b, g2) <- genBytes numBytes g
    return (ULIDRandom b, g2)

-- | Generate a ULID Random based on a standard random number generator.
-- | see: https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
mkULIDRandom :: RandomGen g => g -> (ULIDRandom, g)
mkULIDRandom g = let
    (g1, g2) = split g
    genbytes = (BS.pack) . take numBytes . randoms
    in (ULIDRandom $ genbytes g, g2)

-- | Generate a ULID Random based on the global random number generator.
getULIDRandom :: IO ULIDRandom
getULIDRandom = fst <$> mkULIDRandom <$> newStdGen -- Note: the call to newStdGen splits the generator, so this is safe to call multiple times

instance Show ULIDRandom where
    show (ULIDRandom r) =  (CR.encode) 16.roll.(BS.unpack) $ r

instance Read ULIDRandom where
    readsPrec _ = map (\(c,r)->(ULIDRandom $ (BS.pack) $ unroll numBytes c, r)) . (CR.decode) 16

instance Binary ULIDRandom where
    put (ULIDRandom r) = mapM_ put (BS.unpack $ r)
    get = ULIDRandom <$> (BS.pack) <$> replicateM numBytes get

instance NFData ULIDRandom where
    rnf (ULIDRandom r) = rnf r
