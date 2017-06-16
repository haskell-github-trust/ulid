module Main where

import           Data.ULID

import           Control.DeepSeq
import           Control.Monad            (replicateM)
import qualified Data.Text                as T
import qualified Data.Text.Format.Numbers as FN
import qualified Data.Time.Clock.POSIX    as PX

formatTN = T.unpack . (FN.prettyI (Just ','))

main :: IO ()
main = do
    -- Run many iterations of getULID
    let ops = 100000
    begin <- PX.getPOSIXTime
    ulids <- replicateM ops (getULID >>= return.force)
    end <- PX.getPOSIXTime
    let elapsed = end - begin
    let opsPerSec = (fromIntegral ops) / (realToFrac elapsed) :: Double
    putStr $ formatTN (round opsPerSec)
    putStrLn " op/s generate"

