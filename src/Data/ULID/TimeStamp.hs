module Data.ULID.TimeStamp (
    ULIDTimeStamp,
    mkULIDTimeStamp,
    getULIDTimeStamp
) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Roll
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import qualified Data.ULID.Crockford   as CR

numBytes = 6 -- 48 bits

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
    show (ULIDTimeStamp i) = CR.encode 10 i

instance Read ULIDTimeStamp where
    readsPrec _ = map (\(c,r)->(ULIDTimeStamp c, r)) . (CR.decode) 10

instance Binary ULIDTimeStamp where
    put (ULIDTimeStamp i) = mapM_ put (unroll numBytes i)
    get = ULIDTimeStamp <$> roll <$> replicateM numBytes get
