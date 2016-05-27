module FOMObot.Types.BotConfig where

import Data.ByteString.Char8 (pack, unpack)
import Database.Redis (ConnectInfo(..), PortID(PortNumber), defaultConnectInfo)
import System.Environment (getEnv)
import URI.ByteString
    ( Authority(..)
    , Host(..)
    , Port(..)
    , URI(..)
    , UserInfo(..)
    , parseURI
    , strictURIParserOptions
    )

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
    <*> (parseRedisURL <$> getEnv "REDIS_URL")

parseRedisURL :: String -> ConnectInfo
parseRedisURL url = defaultConnectInfo
    { connectAuth = uiPassword <$> (authorityUserInfo =<< mauth)
    , connectHost = host
    , connectPort = port
    }
  where
    parseURI' = either (const Nothing) Just . parseURI strictURIParserOptions 
    muri = parseURI' $ pack url
    mauth = uriAuthority =<< muri
    host = maybe (connectHost defaultConnectInfo) (unpack . hostBS) $ authorityHost <$> mauth
    port = maybe (connectPort defaultConnectInfo) (PortNumber . fromIntegral . portNumber) $ authorityPort =<< mauth
