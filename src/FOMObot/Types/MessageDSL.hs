module FOMObot.Types.MessageDSL
    ( runMessageDSL
    , shiftInHistory
    , shiftInEvent
    , calcDensity
    , detectEvent
    ) where

import Control.Lens ((^.), (^?!), (&), (.~), (%~), _head, _last, views)
import Control.Monad.Free (Free(..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, get, modify)
import Data.List (nub)
import qualified Web.Slack as Slack

import FOMObot.Helpers.Free
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.DSL
import FOMObot.Types.HistoryItem

data MessageDSL a = ShiftInHistory HistoryItem a
                  | ShiftInEvent Bool a
                  | CalcDensity (Density -> a)
                  | DetectEvent Density (Bool -> a)

instance Functor MessageDSL where
    fmap f (ShiftInHistory h a) = ShiftInHistory h (f a)
    fmap f (ShiftInEvent e a)   = ShiftInEvent e (f a)
    fmap f (CalcDensity g)      = CalcDensity (f . g)
    fmap f (DetectEvent d g)    = DetectEvent d (f . g)

shiftInHistory h    = liftFree $ ShiftInHistory h ()
shiftInEvent e      = liftFree $ ShiftInEvent e ()
calcDensity         = liftFree $ CalcDensity id
detectEvent d       = liftFree $ DetectEvent d id

type MessageProcessor = ReaderT BotConfig (StateT ChannelState (Free DSL))

runMessageDSL :: Free MessageDSL a -> MessageProcessor ()
runMessageDSL (Pure _) = return ()

runMessageDSL (Free (ShiftInHistory historyItem a)) = do
    BotConfig{configHistorySize} <- ask
    mUserId <- (^?! stateHistory . _head . historyUserId) <$> get
    let isFromPreviousUser = mUserId == historyItem ^. historyUserId
    modify $ if isFromPreviousUser
      then
        (& stateHistory . _head .~ historyItem)
      else
        (& stateHistory %~ shiftIn configHistorySize historyItem)
    runMessageDSL a

runMessageDSL (Free (ShiftInEvent event a)) = do
    BotConfig{configDebounceSize} <- ask
    modify (& stateEventHistory %~ shiftIn configDebounceSize event)
    runMessageDSL a

runMessageDSL (Free (CalcDensity g)) = do
    BotConfig{configHistorySize} <- ask
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
    BotConfig{configThreshold} <- ask

    let densitySurpassesThreshold = density > configThreshold
    let atLeastThreeUniqueUsers = views stateHistory ((>=3) . length . nub . (map (^. historyUserId))) state

    runMessageDSL $ g $ and
        [ densitySurpassesThreshold
        , atLeastThreeUniqueUsers
        ]

type Density = Double

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = length xs == size
