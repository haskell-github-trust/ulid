module Main where

import           Data.ULID

-- These imports only needed for cryptographically secure ULID
import qualified Crypto.Random            as CR
import qualified Data.ULID.Random         as UR
import qualified Data.ULID.TimeStamp      as TS

-- These imports only needed for performance measuring function
import           Control.Monad            (replicateM)
import qualified Data.Text                as T
import qualified Data.Text.Format.Numbers as FN
import qualified Data.Time.Clock.POSIX    as PX


main :: IO ()
main = do
     -- Derive a ULID using the current time and default random number generator
    ulid1 <- getULID
    print ulid1

    -- Derive a ULID using a specified time and default random number generator
    ulid2 <- getULIDTime 1469918176.385 -- POSIX Time, specified to the millisecond
    print ulid2


    -- Below only for crypographically secure ULID example

    -- This default instantiation may not be sufficiently secure, see the docs
    -- https://hackage.haskell.org/package/crypto-api-0.13.2/docs/Crypto-Random.html
    g <- (CR.newGenIO :: IO CR.SystemRandom)

    -- Generate time stamp from current time
    t <- TS.getULIDTimeStamp

    let ulid3 = case UR.mkCryptoULIDRandom g of
            Left err        -> error $ show err
            Right (rnd, g2) -> ULID t rnd   -- use g2, etc, to continue generating secure ULIDs
    print ulid3


formatTN = T.unpack . (FN.prettyI (Just ','))

perf :: IO ()
perf = do
    -- Run many iterations of getULID
    let ops = 100000
    begin <- PX.getPOSIXTime
    ulids <- replicateM ops getULID  -- TODO: use deepseq from NFData here
    end <- PX.getPOSIXTime
    let elapsed = end - begin
    let opsPerSec = (fromIntegral ops) / (realToFrac elapsed) :: Double
    putStr $ formatTN (round opsPerSec)
    putStrLn " op/s » generate"

