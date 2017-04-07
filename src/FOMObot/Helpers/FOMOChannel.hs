module FOMObot.Helpers.FOMOChannel
    ( isFOMOChannel
    , alertFOMOChannel
    , alertUsers
    ) where

import Control.Lens (uses, views, view, (^.), review)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.DMChannel (getDMChannel)

isFOMOChannel :: Slack.ChannelId -> Free DSL Bool
isFOMOChannel cid = (== Just cid) <$> getFOMOChannel

getFOMOChannel :: Free DSL (Maybe Slack.ChannelId)
getFOMOChannel = do
    channels <- uses Slack.session $ view Slack.slackChannels
    return $ fromJust $ channelFinder channels
  where
    channelFinder = find (views Slack.channelName (== "fomo"))

alertFOMOChannel :: Slack.ChannelId -> Free DSL ()
alertFOMOChannel cid = do
    fomoChannel <- getFOMOChannel
    maybe (return ()) (`sendMessage` message) getFOMOChannel
  where
    message = "Check out <#" <> (cid ^. Slack.getId) <> ">"

alertUsers :: [String] -> Slack.ChannelId -> Free DSL ()
alertUsers uids cid = mapM_ (\uid -> do
    channel <- getDMChannel uid
    let channelId = (review Slack.getId) . T.pack <$> channel
    maybe (return ()) (`sendMessage` message) channelId) uids
  where
    message = "Check out <#" <> (cid ^. Slack.getId) <> ">"
