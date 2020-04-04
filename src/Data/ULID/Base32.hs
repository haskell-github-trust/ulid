-- | Partly adapted from https://hackage.haskell.org/package/crockford
module Data.ULID.Base32
  ( encode
  , encodeChar
  , decode
  , decodeChar
  )
where

import Data.Char
import Data.Maybe
import Text.Read
import Data.Text as T

import Data.ULID.Digits (digits, unDigits)


-- | Decodes a Crockford base 32 encoded `Text` into an natural number,
-- if possible. Returns `Nothing` if the `Text` is not a valid encoded value.
decodePlain :: Integral i => Text -> Maybe i
decodePlain base32text = do
  numbers <- mapM decodeChar $ T.unpack base32text
  pure $ unDigits 32 numbers


-- | Encodes an natural number into a Text,
-- using Douglas Crockford's base 32 encoding.
-- Returns `Nothing` if number is negative.
encodePlain :: Integral i => i -> Text
encodePlain =
  T.pack . fmap encodeChar . digits 32


-- | Decode a character to its corresponding integer
decodeChar :: Integral i => Char -> Maybe i
decodeChar c = case Data.Char.toUpper c of
    '0' -> Just 0
    'O' -> Just 0
    '1' -> Just 1
    'I' -> Just 1
    'L' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
    'G' -> Just 16
    'H' -> Just 17
    'J' -> Just 18
    'K' -> Just 19
    'M' -> Just 20
    'N' -> Just 21
    'P' -> Just 22
    'Q' -> Just 23
    'R' -> Just 24
    'S' -> Just 25
    'T' -> Just 26
    'V' -> Just 27
    'W' -> Just 28
    'X' -> Just 29
    'Y' -> Just 30
    'Z' -> Just 31
    _ -> Nothing


-- | Encode an integer to its corresponding character
encodeChar :: Integral i => i -> Char
encodeChar i = case i of
    0  -> '0'
    1  -> '1'
    2  -> '2'
    3  -> '3'
    4  -> '4'
    5  -> '5'
    6  -> '6'
    7  -> '7'
    8  -> '8'
    9  -> '9'
    10 -> 'A'
    11 -> 'B'
    12 -> 'C'
    13 -> 'D'
    14 -> 'E'
    15 -> 'F'
    16 -> 'G'
    17 -> 'H'
    18 -> 'J'
    19 -> 'K'
    20 -> 'M'
    21 -> 'N'
    22 -> 'P'
    23 -> 'Q'
    24 -> 'R'
    25 -> 'S'
    26 -> 'T'
    27 -> 'V'
    28 -> 'W'
    29 -> 'X'
    30 -> 'Y'
    31 -> 'Z'
    _  -> '0'


-- | Source: https://stackoverflow.com/a/29153602
-- The safety for m > length was removed, because that should never happen.
-- If it does, it should crash.
leftpad :: Int -> Text -> Text
leftpad m xs =
  T.replicate (m - T.length xs) "0" <> xs


-- | Converts all negative numbers to 0
clampZero :: Integral i => i -> i
clampZero x =
  if x < 0
  then 0
  else x


-- | >>> encode 5 123
-- "0003V"
--
-- | >>> encode (-5) (-123)
-- ""
encode
  :: Integral i
  => Int  -- ^ Overall length of resulting Text
  -> i  -- ^ Natural number to encode
  -> Text  -- ^ 0 padded, Douglas Crockford's base 32 encoded Text
encode width =
  (leftpad $ clampZero width) . encodePlain . clampZero


-- | >>> decode 5 "0003V"
-- [(123,"")]
decode
  :: Integral i
  => Int  -- ^ Overall length of input Text
  -> Text  -- ^ Base 32 encoded Text
  -> [(i, Text)]  -- ^ List of possible parses
decode width str  | T.length str >= width   = let
                      (crock, remainder) = T.splitAt width str
                    in case decodePlain crock of
                        Nothing -> []
                        Just c  -> [(c, remainder)]
                  | otherwise             = []
