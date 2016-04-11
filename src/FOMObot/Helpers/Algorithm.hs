module FOMObot.Helpers.Algorithm
    ( shiftInTime
    , shiftInEvent
    , calcDensity
    , detectFOMOEvent
    ) where

import Control.Lens ((^.))
import qualified Web.Slack as Slack

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState

type Density = Double

calcDensity :: ChannelState -> Bot Density
calcDensity ChannelState{stateHistory} = do
    BotConfig{configHistorySize} <- getConfig
    return $ if isArrayFull stateHistory configHistorySize
        then calc $ fromIntegral configHistorySize
        else 0
  where
    calc historySize = 59 * historySize / timeOverHistory
    timeOverHistory = realToFrac $ (head stateHistory ^. Slack.slackTime) - (last stateHistory ^. Slack.slackTime)

detectFOMOEvent :: Density -> Bot Bool
detectFOMOEvent density = do
    BotConfig{configThreshold} <- getConfig
    return $ density > configThreshold

shiftInTime :: BotConfig -> Slack.SlackTimeStamp -> ChannelState -> ChannelState
shiftInTime BotConfig{configHistorySize} timestamp s@ChannelState{stateHistory} =
    s { stateHistory = shiftIn configHistorySize stateHistory timestamp }

shiftInEvent :: BotConfig -> Bool -> ChannelState -> ChannelState
shiftInEvent BotConfig{configDebounceSize} event s@ChannelState{stateEventHistory} =
    s { stateEventHistory = shiftIn configDebounceSize stateEventHistory event }

shiftIn :: Int -> [a] -> a -> [a]
shiftIn size xs item
    | isArrayFull xs size = item:init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = length xs == size
