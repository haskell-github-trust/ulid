module Data.ULID.Aeson () where

import Data.ULID
import Data.ULID.Random
import Data.Aeson
import Data.Aeson.Encoding (string)
import Data.String (IsString(fromString))
import Data.Aeson.Types (Parser)

instance FromJSON ULID where
    parseJSON value =
         read <$> parseJSON value

instance ToJSON ULID where
    toJSON = fromString . show
    toEncoding = string . show

instance FromJSON ULIDRandom where
    parseJSON value =
         read <$> parseJSON value

instance ToJSON ULIDRandom where
    toJSON = fromString . show
    toEncoding = string . show
