module Main where

import Data.ULID

import Control.DeepSeq
import Control.Monad (replicateM)
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Format.Numbers as FN
import Data.Time.Clock.POSIX as PX


formatTN :: Int -> Text
formatTN =
  FN.prettyI $ Just ','


main :: IO ()
main = do
  -- Run many iterations of getULID
  let ops = 100000
  begin <- PX.getPOSIXTime
  ulids <- replicateM ops (getULID >>= return.force)
  end <- PX.getPOSIXTime
  let elapsed = end - begin
  let opsPerSec = (fromIntegral ops) / (realToFrac elapsed) :: Double
  T.putStr $ formatTN $ round opsPerSec
  T.putStrLn " op/s generate"
