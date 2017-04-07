module FOMObot.Interpretors.MessageDSL
    ( runMessageDSL
    ) where

import Control.Lens ((^.), (^?!), (&), (.~), (%~), _head, _last, views)
import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans.Class (lift)
import Data.List (nub)
import qualified Web.Slack as Slack

import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.DSL
import FOMObot.Types.HistoryItem
import FOMObot.Types.MessageDSL

type MessageProcessor = StateT ChannelState (Free DSL)

runMessageDSL :: Free MessageDSL a -> MessageProcessor ()
runMessageDSL (Pure _) = return ()

runMessageDSL (Free (ShiftInHistory historyItem a)) = do
    BotConfig{configHistorySize} <- lift getConfig
    mUserId <- (^?! stateHistory . _head . historyUserId) <$> get
    let isFromPreviousUser = mUserId == historyItem ^. historyUserId
    modify $ if isFromPreviousUser
      then
        (& stateHistory . _head .~ historyItem)
      else
        (& stateHistory %~ shiftIn configHistorySize historyItem)
    runMessageDSL a

runMessageDSL (Free (ShiftInEvent event a)) = do
    BotConfig{configDebounceSize} <- lift getConfig
    modify (& stateEventHistory %~ shiftIn configDebounceSize event)
    runMessageDSL a

runMessageDSL (Free (CalcDensity g)) = do
    BotConfig{configHistorySize} <- lift getConfig
    s <- get
    runMessageDSL $ g $ if isArrayFull (s ^. stateHistory) configHistorySize
        then calc s $ fromIntegral configHistorySize
        else 0
  where
    calc s historySize = 60 * historySize / timeOverHistory s
    timeOverHistory s = realToFrac $ (latestTimeStamp s) - (earliestTimeStamp s)
    latestTimeStamp s = s ^?! stateHistory . _head . historyTimeStamp . Slack.slackTime
    earliestTimeStamp s = s ^?! stateHistory . _last . historyTimeStamp . Slack.slackTime

runMessageDSL (Free (DetectEvent density g)) = do
    state <- get
    BotConfig{configThreshold} <- lift getConfig

    let densitySurpassesThreshold = density > configThreshold
    let atLeastThreeUniqueUsers = views stateHistory ((>=3) . length . nub . (map (^. historyUserId))) state

    runMessageDSL $ g $ and
        [ densitySurpassesThreshold
        , atLeastThreeUniqueUsers
        ]

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = length xs == size
