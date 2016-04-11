module FOMObot.Types.BotConfig where

import System.Environment (getEnv)

data BotConfig = BotConfig
    { configHistorySize :: Int
    , configDebounceSize :: Int
    , configThreshold :: Double
    }

buildConfig :: IO BotConfig
buildConfig = BotConfig
    <$> (read <$> getEnv "HISTORY_SIZE")
    <*> (read <$> getEnv "FOMO_DEBOUNCE")
    <*> (read <$> getEnv "FOMO_THRESHOLD")
