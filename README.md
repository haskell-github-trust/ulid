# ULID Implementation in Haskell

Lexicographically sortable, 128-bit identifier
with 48-bit timestamp and 80 random bits.
Canonically encoded as a 26 character string,
as opposed to the 36 character UUID.

Original implementation and spec: [github.com/alizain/ulid]

[github.com/alizain/ulid]: https://github.com/alizain/ulid/


```txt
 01an4z07by   79ka1307sr9x4mv3

|----------| |----------------|
 Timestamp       Randomness
  48 bits         80 bits
```


## Universally Unique Lexicographically Sortable Identifier

UUID can be suboptimal for many uses-cases because:

- It isn't the most character efficient way of encoding 128 bits of randomness
- UUID v1/v2 is impractical in many environments,
    as it requires access to a unique, stable MAC address
- UUID v3/v5 requires a unique seed and produces randomly distributed IDs,
    which can cause fragmentation in many data structures
- UUID v4 provides no other information than randomness,
    which can cause fragmentation in many data structures

Instead, herein is proposed ULID:

- 128-bit compatibility with UUID
- 1.21e+24 unique ULIDs per millisecond
- Lexicographically sortable
- Canonically encoded as a 26 character string,
    as opposed to the 36 character UUID
- Uses [Douglas Crockford's base 32] for better efficiency and readability
    (5 bits per character)
- Case insensitive
- No special characters (URL safe)

[Douglas Crockford's base 32]: https://www.crockford.com/base32.html


## Known Issues

- No monotonicity guarantees
    ([official spec](
      https://github.com/ulid/spec?tab=readme-ov-file#monotonicity))
- Lexicographically sorted based on the random component
    if timestamps are the same.
    This causes the sort order to be non-deterministic
    for ULIDs with the same timestamp,
    but is necessary to avoid [incorrect `Map` and `Set` behavior](
      https://github.com/haskell-github-trust/ulid/issues/15#issuecomment-2426847267).


## Usage

A simple usage example:

````haskell
module Main where

import Data.ULID

main :: IO ()
main = do
  -- Derive a ULID using the current time and default random number generator
  ulid1 <- getULID
  print ulid1

  -- Derive a ULID using a specified time and default random number generator
  ulid2 <- getULIDTime 1469918176.385 -- POSIX Time, millisecond precision
  print ulid2
````

As per the spec, it is also possible to use a cryptographically-secure
random number generator to contribute the randomness.
However, the programmer must manage the generator on their own.

Example:

```haskell
module Main where

import Data.ULID

import qualified Crypto.Random       as CR
import qualified Data.ULID.Random    as UR
import qualified Data.ULID.TimeStamp as TS

main :: IO ()
main = do
  g <- (CR.newGenIO :: IO CR.SystemRandom)

  -- Generate timestamp from current time
  t <- TS.getULIDTimeStamp

  let ulid3 = case UR.mkCryptoULIDRandom g of
        Left err        -> error $ show err
        -- Use g2, …, to continue generating secure ULIDs
        Right (rnd, g2) -> ULID t rnd

  print ulid3
```


## Test Suite

```sh
stack test
```


## Performance

```sh
stack bench
```

```txt
Running 1 benchmarks...
Benchmark ulid-bench: RUNNING...
217,868 op/s generate
Benchmark ulid-bench: FINISH
```
