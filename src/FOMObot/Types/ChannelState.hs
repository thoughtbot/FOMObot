module FOMObot.Types.ChannelState where

import qualified Web.Slack as Slack

data ChannelState = ChannelState
    { stateHistory :: [Slack.SlackTimeStamp]
    , stateEventHistory :: [Bool]
    } deriving (Show)
