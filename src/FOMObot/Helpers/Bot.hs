module FOMObot.Helpers.Bot
    ( receiveMessage
    , printBot
    , sendMessage
    , alertFOMOChannel
    , updateState
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Data.HashMap (member, insert, adjust)
import qualified Network.WebSockets as WS

import FOMObot.Types.Message
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Helpers.Time
import FOMObot.Helpers.MovingAverage

receiveMessage :: Bot Message
receiveMessage = do
    conn <- connection
    untilJust $ maybeFilter =<< decode <$> receiveData conn
    where
        receiveData = liftIO . WS.receiveData
        maybeFilter = maybe (return Nothing) filterMessage

filterMessage :: Message -> Bot (Maybe Message)
filterMessage m@Message{..} = ask >>= return . botMessageFilter
    where
        botMessageFilter BotConfig{..}
            | messageType == "message" && messageChannelID /= configChannelID && messageUserID /= configBotID = Just m
            | otherwise = Nothing

printBot :: Show a => a -> Bot ()
printBot = liftIO . print

sendMessage :: String -> String -> Bot ()
sendMessage message channel = liftIO . (`WS.sendTextData` responseData) =<< connection
    where
        responseData = encode $ Message "message" channel "" "" message

alertFOMOChannel :: String -> Bot ()
alertFOMOChannel message = (sendMessage message) =<< configChannelID <$> ask

connection :: Bot WS.Connection
connection = configConnection <$> ask

updateState :: Message -> Bot ()
updateState message = modify =<< (alterState message) <$> ask

alterState :: Message -> BotConfig -> BotState -> BotState
alterState Message{..} config state
    | member messageChannelID state = adjust (updateChannelState config messageTs) messageChannelID state
    | otherwise = insert messageChannelID newChannelState state
    where
        newChannelState = ChannelState 1 0 0 messageTs

updateChannelState :: BotConfig -> String -> ChannelState -> ChannelState
updateChannelState BotConfig{..} ts ChannelState{..} = ChannelState (stateCount + 1) longAvg shortAvg ts
    where
        longAvg = singleExpSmoothing longAlpha stateLongAvg diff
        shortAvg = singleExpSmoothing shortAlpha stateShortAvg diff
        longAlpha = realToFrac configLongAlpha
        shortAlpha = realToFrac configShortAlpha
        diff = diffTime ts stateLastTimeStamp 
