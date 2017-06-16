{- |
Module      : Data.ULID
Copyright   : (c) 2017 Steve Kollmansberger

License     : BSD-style

Maintainer  : steve@kolls.net
Stability   : experimental
Portability : portable

This library implements the Universally Unique Lexicographically Sortable Identifier, as described at <https://github.com/alizain/ulid>.

UUID can be suboptimal for many uses-cases because:

* It isn't the most character efficient way of encoding 128 bits of randomness
* UUID v1/v2 is impractical in many environments, as it requires access to a unique, stable MAC address
* UUID v3/v5 requires a unique seed and produces randomly distributed IDs, which can cause fragmentation in many data structures
* UUID v4 provides no other information than randomness which can cause fragmentation in many data structures

Instead, herein is proposed ULID:

* 128-bit compatibility with UUID
* 1.21e+24 unique ULIDs per millisecond
* Lexicographically sortable!
* Canonically encoded as a 26 character string, as opposed to the 36 character UUID
* Uses Crockford's base32 for better efficiency and readability (5 bits per character)
* Case insensitive
* No special characters (URL safe)

-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.ULID (
    ULID(..),
    getULIDTime,
    getULID
) where

import           Control.DeepSeq
import           Data.Binary
import           Data.Data
import           Data.Monoid           ((<>))
import           Data.Time.Clock.POSIX
import           System.IO.Unsafe
import           System.Random

import           Data.ULID.Random
import           Data.ULID.TimeStamp

data ULID = ULID {
    timeStamp :: !ULIDTimeStamp,
    random    :: !ULIDRandom
    }
    deriving (Eq, Typeable, Data)

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

-- Because of the strictness annotations, this shouldn't be needed and shouldn't do anything
-- I tested and confirmed this in the benchmark, but since I did the work to put it here
-- It's no harm to leave it in
instance NFData ULID where
    rnf (ULID ts bytes) = rnf ts `seq` (rnf bytes `seq` ())

instance Random ULID where
    randomR _ _ = error "No range allowed for random ULID"
    random g = unsafePerformIO $ do
        t <- getULIDTimeStamp
        let (r, g') = mkULIDRandom g
        return (ULID t r, g')
    randomIO = getULID
