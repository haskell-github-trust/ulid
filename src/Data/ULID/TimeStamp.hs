module Data.ULID.TimeStamp (
    ULIDTimeStamp,
    mkULIDTimeStamp,
    getULIDTimeStamp
) where

import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Data.ULID.Crockford

-- UNIX time in milliseconds
newtype ULIDTimeStamp = ULIDTimeStamp Integer
    deriving (Eq, Ord)

-- | Generate a ULID Timestamp based on a specified time
mkULIDTimeStamp :: POSIXTime -- ^ The specified UNIX time (seconds) to millisecond precision, e.g. 1469918176.385
    -> ULIDTimeStamp
mkULIDTimeStamp = ULIDTimeStamp . round . (*1000)

-- | Generate a ULID Timestamp based on current system UNIX time
getULIDTimeStamp :: IO ULIDTimeStamp
getULIDTimeStamp = mkULIDTimeStamp <$> getPOSIXTime

instance Show ULIDTimeStamp where
    show (ULIDTimeStamp i) = encode 10 i

instance Read ULIDTimeStamp where
    readsPrec _ = map (\(c,r)->(ULIDTimeStamp c, r)) . decode 10

