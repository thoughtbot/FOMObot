module FOMObot.Helpers.Bot
    ( processMessage
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (encode)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot

processMessage :: Either String Message -> Bot ()
processMessage = either doNothing printMessage
    where
        doNothing = const $ return ()

printMessage :: Message -> Bot ()
printMessage m@(Message t _ _ _)
    | t == "message" = liftIO $ print m
    | otherwise = return ()

alertChannel :: String -> Bot ()
alertChannel channel = do
    connection <- ask
    liftIO $ WS.sendTextData connection responseData
    where
        responseData = encode message
        message = Message "message" channel "" $ concat ["Check out <#", channel, ">"]
