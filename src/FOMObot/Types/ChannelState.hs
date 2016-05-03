module FOMObot.Types.ChannelState where

import Control.Lens (makeLenses)
import FOMObot.Types.HistoryItem

data ChannelState = ChannelState
    { _stateHistory :: [HistoryItem]
    , _stateEventHistory :: [Bool]
    } deriving (Show)

makeLenses ''ChannelState
