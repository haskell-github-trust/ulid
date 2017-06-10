# Ulid implementation in Haskell

Lexicographically sortable, 128-bit identifier with 48-bit timestamp and 80 random bits.
Canonically encoded as a 26 character string, as opposed to the 36 character UUID.

Original implementation and spec: https://github.com/alizain/ulid/


# Universally Unique Lexicographically Sortable Identifier

UUID can be suboptimal for many uses-cases because:

- It isn't the most character efficient way of encoding 128 bits of randomness
- UUID v1/v2 is impractical in many environments, as it requires access to a unique, stable MAC address
- UUID v3/v5 requires a unique seed and produces randomly distributed IDs, which can cause fragmentation in many data structures
- UUID v4 provides no other information than randomness which can cause fragmentation in many data structures

Instead, herein is proposed ULID:

- 128-bit compatibility with UUID
- 1.21e+24 unique ULIDs per millisecond
- Lexicographically sortable!
- Canonically encoded as a 26 character string, as opposed to the 36 character UUID
- Uses Crockford's base32 for better efficiency and readability (5 bits per character)
- Case insensitive
- No special characters (URL safe)


## Usage

A simple usage example:

````haskell
module Main where

import           Data.ULID

main :: IO ()
main = do
    -- Derive a ULID using the current time and default random number generator
    ulid1 <- getULID
    print ulid1

    -- Derive a ULID using a specified time and default random number generator
    ulid2 <- getULIDTime 1469918176.385 -- POSIX Time, specified to the millisecond
    print ulid2
````

As per the spec, it is also possible to use a cryptographically-secure random number generator to contribute the randomness.  However, the programmer must manage the generator on their own. Example:


````haskell
module Main where

import           Data.ULID

import qualified Crypto.Random       as CR
import qualified Data.ULID.Random    as UR
import qualified Data.ULID.TimeStamp as TS

main :: IO ()
main = do     
    -- This default instantiation may not be sufficiently secure, see the docs 
    -- https://hackage.haskell.org/package/crypto-api-0.13.2/docs/Crypto-Random.html
    g <- (CR.newGenIO :: IO CR.SystemRandom)

    -- Generate time stamp from current time
    t <- TS.getULIDTimeStamp
    
    let ulid3 = case UR.mkCryptoULIDRandom g of
            Left err        -> error $ show err
            Right (rnd, g2) -> ULID t rnd   -- use g2, etc, to continue generating secure ULIDs
    print ulid3
````