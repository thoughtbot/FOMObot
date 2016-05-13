module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    ) where

import Control.Lens (uses, views, view, (^.))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Types.Bot

isFOMOChannel :: Slack.ChannelId -> Bot Bool
isFOMOChannel cid = views Slack.channelId (== cid) <$> getFOMOChannel

getFOMOChannel :: Bot Slack.Channel
getFOMOChannel = do
    channels <- uses Slack.session $ view Slack.slackChannels
    return $ fromJust $ channelFinder channels
  where
    channelFinder = find (views Slack.channelName (== "fomo"))

alertFOMOChannel :: Slack.ChannelId -> Bot ()
alertFOMOChannel channelID = do
    fomoChannel <- view Slack.channelId <$> getFOMOChannel
    Slack.sendMessage fomoChannel message
  where
    message = "Check out <#" <> (channelID ^. Slack.getId) <> ">"
