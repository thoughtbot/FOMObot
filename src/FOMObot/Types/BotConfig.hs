module FOMObot.Types.BotConfig where

import qualified Network.WebSockets as WS

type PartialConfig = WS.Connection -> BotConfig

data BotConfig = BotConfig
    { configChannelID :: String
    , configBotID :: String
    , configHistorySize :: Int
    , configFOMODebounce :: Int
    , configFOMOThreshold :: Double
    , configConnection :: WS.Connection
    }
