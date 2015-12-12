module FOMObot.Types.Message where

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, (.:), (.=), Value(..), object)
import Data.Aeson.Types (typeMismatch)

data Message = Message
    { _type :: String
    , _channel :: String
    , _ts :: String
    , _text :: String
    } deriving (Show)

instance FromJSON Message where
    parseJSON (Object o) = Message
        <$> o .: "type"
        <*> o .: "channel"
        <*> o .: "ts"
        <*> o .: "text"

    parseJSON invalid = typeMismatch "Message" invalid

instance ToJSON Message where
    toJSON message = object
        [ "type"    .= _type message
        , "channel" .= _channel message
        , "text"    .= _text message
        ]
