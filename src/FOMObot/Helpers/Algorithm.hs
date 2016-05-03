module FOMObot.Helpers.Algorithm
    ( shiftInHistory
    , shiftInEvent
    , detectFOMOEvent
    ) where

import Control.Lens ((^.), (^?), (^?!), (&), (.~), (%~), _head, _last, views)
import Data.List (nub)
import qualified Web.Slack as Slack

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

type Density = Double

calcDensity :: ChannelState -> Bot Density
calcDensity s = do
    BotConfig{configHistorySize} <- getConfig
    return $ if isArrayFull (s ^. stateHistory) configHistorySize
        then calc $ fromIntegral configHistorySize
        else 0
  where
    calc historySize = 60 * historySize / timeOverHistory
    timeOverHistory = realToFrac $ latestTimeStamp - earliestTimeStamp
    latestTimeStamp = s ^?! stateHistory . _head . historyTimeStamp . Slack.slackTime
    earliestTimeStamp  = s ^?! stateHistory . _last . historyTimeStamp . Slack.slackTime

detectFOMOEvent :: ChannelState -> Bot Bool
detectFOMOEvent state = do
    densitySurpassesThreshold <- (>) <$> calcDensity state <*> (configThreshold <$> getConfig)
    return $ and
        [ densitySurpassesThreshold
        , atLeastThreeUniqueUsers
        ]
  where
    atLeastThreeUniqueUsers = views stateHistory ((>=3) . length . nub . (map (^. historyUserId))) state

shiftInHistory :: BotConfig -> HistoryItem -> ChannelState -> ChannelState
shiftInHistory BotConfig{configHistorySize} historyItem s =
    if isFromPreviousUser
      then
        s & stateHistory . _head .~ historyItem
      else
        s & stateHistory %~ shiftIn configHistorySize historyItem
  where
    isFromPreviousUser = (s ^? stateHistory . _head . historyUserId) == Just (historyItem ^. historyUserId)

shiftInEvent :: BotConfig -> Bool -> ChannelState -> ChannelState
shiftInEvent BotConfig{configDebounceSize} event s =
    s & stateEventHistory %~ shiftIn configDebounceSize event

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = length xs == size
