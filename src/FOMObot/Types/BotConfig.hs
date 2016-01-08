module FOMObot.Types.BotConfig where

import qualified Network.WebSockets as WS

type PartialConfig = WS.Connection -> BotConfig

data BotConfig = BotConfig
    { _channelID :: String
    , _connection :: WS.Connection
    }
