{-# LANGUAGE Rank2Types #-}
module FOMObot.Interpretors.DSL
    ( safeRunDSL
    ) where

import Control.Lens ((^.), (^?!), view, _head, uses)
import Control.Monad.Free (Free(..))
import qualified Data.Text as T
import qualified Web.Slack as Slack

import FOMObot.Helpers.CommandProcessor
import FOMObot.Helpers.FOMOChannel
import FOMObot.Helpers.Preferences
import FOMObot.Types.AppState
import FOMObot.Types.Bot
import FOMObot.Types.ChannelState
import FOMObot.Types.DSL

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
