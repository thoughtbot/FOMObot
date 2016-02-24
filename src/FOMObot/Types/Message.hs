module FOMObot.Types.Message where

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, (.:), (.=), Value(..), object)
import Data.Aeson.Types (typeMismatch)

import FOMObot.Types.TimeStamp

data Message = Message
    { messageType :: String
    , messageChannelID :: String
    , messageUserID :: String
    , messageTs :: TimeStamp
    , messageText :: String
    } deriving (Show)

instance FromJSON Message where
    parseJSON (Object o) = Message
        <$> o .: "type"
        <*> o .: "channel"
        <*> o .: "user"
        <*> o .: "ts"
        <*> o .: "text"

    parseJSON invalid = typeMismatch "Message" invalid

instance ToJSON Message where
    toJSON Message{..} = object
        [ "type"    .= messageType
        , "channel" .= messageChannelID
        , "text"    .= messageText
        ]
