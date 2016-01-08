module FOMObot.Types.RTMStartResponse where

import Data.Aeson (FromJSON, parseJSON, (.:), Value(..))
import Data.Aeson.Types (typeMismatch)

import FOMObot.Types.Channel

data RTMStartResponse = RTMStartResponse
    { _url :: String
    , _selfID :: String
    , _channels :: [Channel]
    } deriving (Show)

instance FromJSON RTMStartResponse where
    parseJSON (Object o) = RTMStartResponse
        <$> o .: "url"
        <*> (o .: "self" >>= (.: "id"))
        <*> o .: "channels"

    parseJSON invalid = typeMismatch "RTMStartResponse" invalid
