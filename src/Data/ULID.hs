module Data.ULID (
    ULID(..),
    getULIDTime,
    getULID
) where

import           Data.Binary
import           Data.Monoid           ((<>))
import           Data.Time.Clock.POSIX

import           Data.ULID.Random
import           Data.ULID.TimeStamp

data ULID = ULID {
    timeStamp :: !ULIDTimeStamp,
    random    :: !ULIDRandom
    }
    deriving (Eq)

-- | Derive a ULID using a specified time and default random number generator
getULIDTime :: POSIXTime    -- ^ The specified UNIX time (seconds) to millisecond precision, e.g. 1469918176.385
    -> IO ULID
getULIDTime t = do
    let t' = mkULIDTimeStamp t
    r <- getULIDRandom
    return $ ULID t' r

-- | Derive a ULID using the current time and default random number generator
getULID :: IO ULID
getULID = do
    t <- getULIDTimeStamp
    r <- getULIDRandom
    return $ ULID t r

instance Ord ULID where
    compare (ULID ts1 _) (ULID ts2 _) = compare ts1 ts2

instance Show ULID where
    show (ULID ts bytes) = (show ts) ++ (show bytes)

instance Read ULID where
    readsPrec _ str = do
        (ts, str2) <- reads str
        (rn, str3) <- reads str2
        return (ULID ts rn, str3)

instance Binary ULID where
    put (ULID ts bytes) = put ts <> put bytes
    get = do
        ts <- get
        bytes <- get
        return $ ULID ts bytes
