module FOMObot.Types.BotState where

import qualified Data.HashMap as HM

import FOMObot.Types.ChannelState

type BotState = HM.Map String ChannelState

emptyState :: BotState
emptyState = HM.empty
