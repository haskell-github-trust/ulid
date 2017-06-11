module Data.ULID.Crockford (encode, decode) where

import qualified Codec.Crockford as CR
import           Text.Read

-- source: https://stackoverflow.com/a/29153602
-- I removed the safety for m > length because that should never happen
-- and if it does, I want it to crash!
leftpad m xs = replicate (m - length xs) '0' ++ xs

encode :: Int -> Integer -> String
encode width = (leftpad width).(CR.encode)

decode :: Int -> ReadS Integer
decode width str  | length str >= width   = let
                    (crock, remainder) = splitAt width str
                    in case CR.decode crock of
                        Nothing -> []
                        Just c  -> [(c, remainder)]
                  | otherwise             = []
