module Data.ULID.Crockford (encode, decode) where

import qualified Codec.Crockford as CR
import           Text.Read

-- source: https://stackoverflow.com/a/29153602
leftpad m xs = replicate (m - length ys) '0' ++ ys
    where ys = take m xs

encode :: Int -> Integer -> String
encode width = (leftpad width).(CR.encode)

decode :: Int -> ReadS Integer
decode width str  | length str >= width   = let
                    (crock, remainder) = splitAt width str
                    in case CR.decode crock of
                        Nothing -> []
                        Just c  -> [(c, remainder)]
                  | otherwise             = []
