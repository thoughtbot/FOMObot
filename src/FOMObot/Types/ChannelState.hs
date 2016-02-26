module FOMObot.Types.ChannelState where

import FOMObot.Types.TimeStamp

data ChannelState = ChannelState
    { stateHistory :: [TimeStamp]
    , stateEventHistory :: [Bool]
    } deriving (Show)
