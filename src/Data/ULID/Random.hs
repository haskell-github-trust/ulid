module Data.ULID.Random (
    ULIDRandom,
    mkCryptoULIDRandom,
    mkULIDRandom,
    getULIDRandom
) where

import           Crypto.Random
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.List           (foldl', unfoldr)
import           Data.Word
import           System.Random


import qualified Data.ULID.Crockford as CR


newtype ULIDRandom = ULIDRandom BS.ByteString
    deriving (Eq)

-- | Generate a ULID Random based on a cryptographically secure random number generator. 
-- | see: https://hackage.haskell.org/package/crypto-api-0.13.2/docs/Crypto-Random.html
mkCryptoULIDRandom :: CryptoRandomGen g => g -> Either GenError (ULIDRandom, g)
mkCryptoULIDRandom g = do
    (b, g2) <- genBytes 10 g
    return (ULIDRandom b, g2)

-- | Generate a ULID Random based on a standard random number generator.
-- | see: https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
mkULIDRandom :: RandomGen g => g -> (ULIDRandom, g)
mkULIDRandom g = let
    (g1, g2) = split g
    genbytes = (BS.pack) . take 10 . randoms
    in (ULIDRandom $ genbytes g, g2)

-- | Generate a ULID Random based on the global random number generator.
getULIDRandom :: IO ULIDRandom
getULIDRandom = fst <$> mkULIDRandom <$> newStdGen -- Note: the call to newStdGen splits the generator, so this is safe to call multiple times


-- source: http://hackage.haskell.org/package/binary-0.8.5.1/docs/src/Data-Binary-Class.html#line-311
--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

instance Show ULIDRandom where
    show (ULIDRandom r) =  (CR.encode) 16.roll.(BS.unpack) $ r

instance Read ULIDRandom where
    readsPrec _ = map (\(c,r)->(ULIDRandom $ (BS.pack) $ unroll c, r)) . (CR.decode) 16
