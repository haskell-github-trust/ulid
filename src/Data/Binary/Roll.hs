module Data.Binary.Roll where

import           Data.Binary
import           Data.Bits
import           Data.List   (foldl', unfoldr)


-- | unroll and produce an exact number of bytes (left-pad with 0 bytes)
unroll :: Int -> Integer -> [Word8]
unroll bytes val = replicate (bytes - length xs) 0 ++ xs
    where xs = unroll' val

-- source: http://hackage.haskell.org/package/binary-0.8.5.1/docs/src/Data-Binary-Class.html#line-311
--
-- Fold and unfold an Integer to and from a list of its bytes
--
-- MOST SIGNIFICANT BYTE FIRST please (this is a change from the default Data.Binary impl)

unroll' :: Integer -> [Word8]
unroll' = reverse . unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll = foldl' unstep 0
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b
