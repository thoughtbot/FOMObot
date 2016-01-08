module FOMObot.Helpers.Bot
    ( receiveMessage
    , printMessage
    , sendMessage
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot

receiveMessage :: Bot Message
receiveMessage = liftIO . untilJust . (decodeMessage <$>) . WS.receiveData =<< ask
    where
        decodeMessage a = decode a >>= filterMessage

filterMessage :: Message -> Maybe Message
filterMessage m@(Message t _ _ _)
    | t == "message" = Just m
    | otherwise = Nothing

printMessage :: Message -> Bot ()
printMessage = liftIO . print

sendMessage :: String -> String -> Bot ()
sendMessage message channel = do
    connection <- ask
    liftIO $ WS.sendTextData connection responseData
    where
        responseData = encode $ Message "message" channel "" message
