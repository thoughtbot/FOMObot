module FOMObot.Helpers.Bot
    ( receiveMessage
    , printMessage
    , sendMessage
    , alertFOMOChannel
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

receiveMessage :: Bot Message
receiveMessage = untilJust . maybeFilter . (decode <$>) . receiveData =<< connection
    where
        receiveData = liftIO . WS.receiveData
        maybeFilter = (maybe (return Nothing) filterMessage =<<)

filterMessage :: Message -> Bot (Maybe Message)
filterMessage m@Message{..} = ask >>= return . botMessageFilter
    where
        botMessageFilter config
            | _type == "message" && _channel /= _channelID config && _user /= _botID config = Just m
            | otherwise = Nothing

printMessage :: Message -> Bot ()
printMessage = liftIO . print

sendMessage :: String -> String -> Bot ()
sendMessage message channel = liftIO . (`WS.sendTextData` responseData) =<< connection
    where
        responseData = encode $ Message "message" channel "" "" message

alertFOMOChannel :: String -> Bot ()
alertFOMOChannel message = (sendMessage message) =<< _channelID <$> ask

connection :: Bot WS.Connection
connection = _connection <$> ask
