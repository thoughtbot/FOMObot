module FOMObot.Helpers.Bot
    ( receiveMessage
    , printBot
    , sendMessage
    , alertFOMOChannel
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Data.Time (getCurrentTime)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.TimeStamp

receiveMessage :: Bot Message
receiveMessage = untilJust $ maybeFilter =<< decode <$> receiveData
    where
        receiveData = liftIO . WS.receiveData =<< connection
        maybeFilter = maybe (return Nothing) filterMessage

filterMessage :: Message -> Bot (Maybe Message)
filterMessage m@Message{messageType, messageChannelID, messageUserID} =
    ask >>= return . botMessageFilter
    where
        botMessageFilter BotConfig{configChannelID, configBotID}
            | messageType == "message" && messageChannelID /= configChannelID && messageUserID /= configBotID = Just m
            | otherwise = Nothing

printBot :: Show a => a -> Bot ()
printBot = liftIO . print

sendMessage :: String -> String -> Bot ()
sendMessage message channel = liftIO =<< WS.sendTextData <$> connection <*> (responseData <$> currentTimeStamp)
    where
        responseData now = encode $ Message "message" channel "" now message
        currentTimeStamp = TimeStamp <$> liftIO getCurrentTime

alertFOMOChannel :: String -> Bot ()
alertFOMOChannel message = (sendMessage message) =<< configChannelID <$> ask

connection :: Bot WS.Connection
connection = configConnection <$> ask
