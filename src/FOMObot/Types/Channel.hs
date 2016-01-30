module FOMObot.Types.Channel where

import Data.Aeson (FromJSON, parseJSON, (.:), Value(..))
import Data.Aeson.Types (typeMismatch)

data Channel = Channel
    { channelID :: String
    , channelName :: String
    } deriving (Show)

instance FromJSON Channel where
    parseJSON (Object o) = Channel
        <$> o .: "id"
        <*> o .: "name"

    parseJSON invalid = typeMismatch "Channel" invalid
