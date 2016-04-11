module FOMObot.Helpers.Bot
    ( isFOMOChannel
    , processMessage
    , alertFOMOChannel
    ) where

import Control.Lens (uses, views, view, (^.))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.Algorithm
import FOMObot.Types.Bot
import FOMObot.Types.ChannelState

isFOMOChannel :: Slack.ChannelId -> Bot Bool
isFOMOChannel cid = views Slack.channelId (== cid) <$> getFOMOChannel

getFOMOChannel :: Bot Slack.Channel
getFOMOChannel = do
    channels <- uses Slack.session $ view Slack.slackChannels
    return $ fromJust $ channelFinder channels
  where
    channelFinder = find (views Slack.channelName (== "fomo"))

processMessage :: Slack.Event -> Bot Bool
processMessage (Slack.Message channelID _ _ messageTimestamp _ _) = do
    config <- getConfig
    let messageChannelID = T.unpack $ channelID ^. Slack.getId

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

processMessage _ = return False

alertFOMOChannel :: Slack.ChannelId -> Bot ()
alertFOMOChannel channelID = do
    fomoChannel <- view Slack.channelId <$> getFOMOChannel
    Slack.sendMessage fomoChannel message
  where
    message = "Check out <#" <> (channelID ^. Slack.getId) <> ">"
