module FOMObot.Types.ChannelState where

import Data.Time.Clock (NominalDiffTime)

data ChannelState = ChannelState
    { stateCount :: Int
    , stateLongAvg :: NominalDiffTime
    , stateShortAvg :: NominalDiffTime
    , stateLastTimeStamp :: String
    } deriving (Show)
