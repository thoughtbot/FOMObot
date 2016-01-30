module FOMObot.Types.Message where

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, (.:), (.=), Value(..), object)
import Data.Aeson.Types (typeMismatch)

data Message = Message
    { messageType :: String
    , messageChannelID :: String
    , messageUserID :: String
    , messageTs :: String
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
