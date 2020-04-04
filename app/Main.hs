module Main where

import Data.ULID

-- These imports only needed for cryptographically secure ULID
import qualified Crypto.Random       as CR
import qualified Data.ULID.Random    as UR
import qualified Data.ULID.TimeStamp as TS


main :: IO ()
main = do
  -- Derive a ULID using the current time and default random number generator
  ulid1 <- getULID
  print ulid1

  -- Derive a ULID using a specified time and default random number generator
  ulid2 <- getULIDTime 1469918176.385 -- ^ POSIX Time in milliseconds
  print ulid2

  -- Below only for cryptographically secure ULID example

  -- This default instantiation may not be sufficiently secure, see the docs
  -- https://hackage.haskell.org/package/crypto-api-0.13.3/docs/Crypto-Random.html
  g <- (CR.newGenIO :: IO CR.SystemRandom)

  -- Generate time stamp from current time
  t <- TS.getULIDTimeStamp

  let ulid3 = case UR.mkCryptoULIDRandom g of
        Left err        -> error $ show err
        -- Use g2, etc, to continue generating secure ULIDs
        Right (rnd, g2) -> ULID t rnd

  print ulid3
