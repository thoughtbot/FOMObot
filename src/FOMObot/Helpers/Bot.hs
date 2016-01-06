module FOMObot.Helpers.Bot
    ( receiveMessage
    , printMessage
    , sendMessage
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (eitherDecode, encode)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot

receiveMessage :: Bot Message
receiveMessage = do
    connection <- ask
    message <- liftIO $ WS.receiveData connection
    either doAgain return $ eitherDecode message
    where
        doAgain = const receiveMessage

printMessage :: Message -> Bot ()
printMessage m@(Message t _ _ _)
    | t == "message" = liftIO $ print m
    | otherwise = return ()

sendMessage :: String -> String -> Bot ()
sendMessage message channel = do
    connection <- ask
    liftIO $ WS.sendTextData connection responseData
    where
        responseData = encode $ Message "message" channel "" message
