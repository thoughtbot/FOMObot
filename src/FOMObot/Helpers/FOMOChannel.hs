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
    message = "<!here> There's a party in <#" <> (channelID ^. Slack.getId) <> ">!"

alertUsers :: [String] -> Slack.ChannelId -> Bot ()
alertUsers uids cid = mapM_ (\uid -> do
    channel <- getDMChannel uid
    let channelId = (review Slack.getId) . T.pack <$> channel
    maybe (return ()) (`Slack.sendMessage` message) channelId) uids
  where
    message = "There's a party in <#" <> (cid ^. Slack.getId) <> ">!"
