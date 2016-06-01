{-# LANGUAGE Rank2Types #-}
module FOMObot.Types.DSL
    ( DSL
    , safeRunDSL
    , processCommandF
    , getBotConfig
    , getChannelState
    , saveChannelState
    , getEventStatus
    , alert
    , end
    ) where

import Control.Lens ((^.), (^?!), view, _head, uses)
import Control.Monad.Free (Free(..))
import qualified Data.Text as T
import qualified Web.Slack as Slack

import FOMObot.Helpers.CommandProcessor
import FOMObot.Helpers.FOMOChannel
import FOMObot.Helpers.Free
import FOMObot.Helpers.Preferences
import FOMObot.Types.AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState

data DSL a = ProcessCommand Slack.Event a
           | GetBotConfig (BotConfig -> a)
           | GetChannelState Slack.ChannelId (ChannelState -> a)
           | SaveChannelState Slack.ChannelId ChannelState a
           | GetEventStatus Slack.ChannelId (Bool -> a)
           | Alert Slack.ChannelId a
           | End

instance Functor DSL where
    fmap f (ProcessCommand m a)     = ProcessCommand m (f a)
    fmap f (GetBotConfig g)         = GetBotConfig (f . g)
    fmap f (GetChannelState c g)    = GetChannelState c (f . g)
    fmap f (SaveChannelState c s a) = SaveChannelState c s (f a)
    fmap f (GetEventStatus c g)     = GetEventStatus c (f . g)
    fmap f (Alert c a)              = Alert c (f a)
    fmap _ End                      = End

processCommandF m       = liftFree $ ProcessCommand m ()
getBotConfig            = liftFree $ GetBotConfig id
getChannelState c       = liftFree $ GetChannelState c id
saveChannelState c s    = liftFree $ SaveChannelState c s ()
getEventStatus c        = liftFree $ GetEventStatus c id
alert c                 = liftFree $ Alert c ()
end                     = liftFree End

safeRunDSL :: (forall a. Free DSL a) -> Bot ()
safeRunDSL = runDSL

runDSL :: Free DSL a -> Bot ()
runDSL (Pure _) = return ()
runDSL (Free End) = return ()

runDSL (Free (ProcessCommand m a)) = do
    processCommand m
    runDSL a

runDSL (Free (GetBotConfig g)) = do
    config <- uses Slack.userState $ view botConfig
    runDSL $ g config

runDSL (Free (GetChannelState cid g)) = do
    channelState <- botChannelState cid
    runDSL $ g channelState

runDSL (Free (SaveChannelState cid state a)) = do
    botSaveState cid state
    runDSL a

runDSL (Free (GetEventStatus cid g)) = do
    channelState <- botChannelState cid
    let eventStatus = channelState ^?! (stateEventHistory . _head)
    runDSL $ g eventStatus

runDSL (Free (Alert c a)) = do
    alertFOMOChannel c
    users <- getUsersForChannel $ T.unpack $ c ^. Slack.getId
    alertUsers users c
    runDSL a
