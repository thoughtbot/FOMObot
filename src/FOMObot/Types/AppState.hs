module FOMObot.Types.AppState where

import Control.Lens

import FOMObot.Types.BotConfig
import FOMObot.Types.BotState

data AppState = AppState
    { _botConfig :: BotConfig
    , _botState :: BotState
    }

makeLenses ''AppState
