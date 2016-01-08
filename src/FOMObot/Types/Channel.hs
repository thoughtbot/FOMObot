module FOMObot.Types.Channel where

import Data.Aeson (FromJSON, parseJSON, (.:), Value(..))
import Data.Aeson.Types (typeMismatch)

data Channel = Channel
    { _id :: String
    , _name :: String
    } deriving (Show)

instance FromJSON Channel where
    parseJSON (Object o) = Channel
        <$> o .: "id"
        <*> o .: "name"

    parseJSON invalid = typeMismatch "Channel" invalid
