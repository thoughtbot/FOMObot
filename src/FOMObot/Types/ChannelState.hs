module FOMObot.Types.ChannelState where

import FOMObot.Types.TimeStamp

data ChannelState = ChannelState
    { stateHistory :: [TimeStamp]
    , stateFOMOHistory :: [Bool]
    } deriving (Show)
