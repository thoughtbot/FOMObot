module FOMObot.Types.BotConfig where

import Control.Exception (try, IOException)
import Data.ByteString.Char8 (pack)
import Database.Redis (ConnectInfo(ConnInfo), PortID(PortNumber))
import System.Environment (getEnv)

data BotConfig = BotConfig
    { configHistorySize :: Int
    , configDebounceSize :: Int
    , configThreshold :: Double
    , configRedisConnection :: ConnectInfo
    }

buildConfig :: IO BotConfig
buildConfig = BotConfig
    <$> (read <$> getEnv "HISTORY_SIZE")
    <*> (read <$> getEnv "FOMO_DEBOUNCE")
    <*> (read <$> getEnv "FOMO_THRESHOLD")
    <*> (ConnInfo
            <$> getEnv "REDIS_HOST"
            <*> (PortNumber . fromInteger . read <$> getEnv "REDIS_PORT")
            <*> (either (const Nothing) (Just . pack)
                <$> (try $ getEnv "REDIS_PASS" :: IO (Either IOException String)))
            <*> (read <$> getEnv "REDIS_DATABASE")
            <*> return 10
            <*> return 30)
