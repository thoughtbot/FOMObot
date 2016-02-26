module FOMObot.Helpers.Bot
    ( receiveMessage
    , processMessage
    , printBot
    , sendMessage
    , alertFOMOChannel
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import qualified Network.WebSockets as WS

import FOMObot.Helpers.Algorithm
import FOMObot.Types.Message
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.TimeStamp

receiveMessage :: Bot Message
receiveMessage = untilJust $ maybeFilter =<< decode <$> receiveData
    where
        receiveData = liftIO . WS.receiveData =<< botConnection
        maybeFilter = maybe (return Nothing) filterMessage

filterMessage :: Message -> Bot (Maybe Message)
filterMessage m@Message{messageType, messageChannelID, messageUserID} =
    ask >>= return . botMessageFilter
    where
        botMessageFilter BotConfig{configChannelID, configBotID}
            | messageType == "message" && messageChannelID /= configChannelID && messageUserID /= configBotID = Just m
            | otherwise = Nothing

processMessage :: Message -> Bot Bool
processMessage Message{messageChannelID, messageTimestamp} = do
    config <- ask
    -- Add the message timestamp to the channel state
    channelState <- shiftInTime config messageTimestamp
        <$> botChannelState messageChannelID

    -- Calculate the density over the timestamps within channel state
    density <- calcDensity channelState
    -- Detect an event that surpasses the threshold
    eventOccurred <- detectFOMOEvent density

    -- Save the channel state after adding the event status
    botSaveState messageChannelID
        $ shiftInEvent config eventOccurred channelState

    -- Signal an event only if an event occured and no recent events
    let recentlyNotified = or $ stateEventHistory channelState
    return $ eventOccurred && not recentlyNotified

sendMessage :: String -> String -> Bot ()
sendMessage message channel = liftIO =<< WS.sendTextData <$> botConnection <*> (responseData <$> currentTimeStamp)
    where
        responseData now = encode $ Message "message" channel "" now message
        currentTimeStamp = TimeStamp <$> liftIO getCurrentTime

alertFOMOChannel :: String -> Bot ()
alertFOMOChannel messageChannelID = sendMessage message =<< configChannelID <$> ask
    where
        message = "Check out <#" <> messageChannelID <> ">"
