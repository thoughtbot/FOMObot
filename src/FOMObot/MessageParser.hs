module FOMObot.MessageParser
    ( parseMessage
    ) where

import Data.Aeson (eitherDecode, FromJSON, parseJSON, (.:), Value(..))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BSL (ByteString)

data Message = Message
    { _type :: String
    , _channel :: String
    , _ts :: String
    } deriving (Show)

instance FromJSON Message where
    parseJSON (Object o) = Message
        <$> o .: "type"
        <*> o .: "channel"
        <*> o .: "ts"

    parseJSON invalid = typeMismatch "Message" invalid

parseMessage :: BSL.ByteString -> Either String Message
parseMessage = eitherDecode

