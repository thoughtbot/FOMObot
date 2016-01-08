module FOMObot.Types.BotConfig where

import qualified Network.WebSockets as WS

data BotConfig = BotConfig
    { _connection :: WS.Connection
    }
