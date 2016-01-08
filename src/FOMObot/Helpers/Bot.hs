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
receiveMessage = let decodeMessage a = decode a >>= filterMessage
    in liftIO . untilJust . (decodeMessage <$>) . WS.receiveData =<< ask

filterMessage :: Message -> Maybe Message
filterMessage m@(Message t _ _ _)
    | t == "message" = Just m
    | otherwise = Nothing

printMessage :: Message -> Bot ()
printMessage = liftIO . print

sendMessage :: String -> String -> Bot ()
sendMessage message channel = let responseData = encode $ Message "message" channel "" message
    in liftIO . (`WS.sendTextData` responseData) =<< ask
