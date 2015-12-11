module FOMObot.Types.RTMStartResponse where

import Data.Aeson (FromJSON, parseJSON, (.:), Value(..))
import Data.Aeson.Types (typeMismatch)

data RTMStartResponse = RTMStartResponse
    { _url :: String
    } deriving (Show)

instance FromJSON RTMStartResponse where
    parseJSON (Object o) = RTMStartResponse
        <$> o .: "url"

    parseJSON invalid = typeMismatch "RTMStartResponse" invalid
