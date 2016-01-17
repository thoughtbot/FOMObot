module FOMObot.Types.ChannelState where

import Data.Time.Clock (NominalDiffTime)

data ChannelState = ChannelState
    { _count :: Int
    , _longAvg :: NominalDiffTime
    , _shortAvg :: NominalDiffTime
    , _lastTimeStamp :: String
    } deriving (Show)
