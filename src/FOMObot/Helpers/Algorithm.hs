module FOMObot.Helpers.Algorithm
    ( shiftInTime
    , shiftInEvent
    , calcDensity
    , detectFOMOEvent
    ) where

import Control.Monad.Reader (ask)
import Data.Time.Clock (diffUTCTime)

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.TimeStamp

type Density = Double

calcDensity :: ChannelState -> Bot Density
calcDensity ChannelState{stateHistory} = do
    BotConfig{configHistorySize} <- ask
    return $ if isArrayFull stateHistory configHistorySize
        then calc $ fromIntegral configHistorySize
        else 0
    where
        calc historySize = 60 * historySize / timeOverHistory
        timeOverHistory = realToFrac $ diffUTCTime (utc $ head stateHistory) (utc $ last stateHistory)

detectFOMOEvent :: Density -> Bot Bool
detectFOMOEvent density = do
    BotConfig{configThreshold} <- ask
    return $ density > configThreshold

shiftInTime :: BotConfig -> TimeStamp -> ChannelState -> ChannelState
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
