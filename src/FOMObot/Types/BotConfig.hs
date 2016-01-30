module FOMObot.Types.BotConfig where

import qualified Network.WebSockets as WS

type PartialConfig = WS.Connection -> BotConfig

data BotConfig = BotConfig
    { configChannelID :: String
    , configBotID :: String
    , configLongAlpha :: Double
    , configShortAlpha :: Double
    , configConnection :: WS.Connection
    }
