{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FOMObot.Helpers.Redis where

import Control.Monad.IO.Class (liftIO)
import Database.Redis (MonadRedis(..), runRedis, connect)

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

instance MonadRedis Bot where
    liftRedis f = do
        BotConfig{configRedisConnection} <- getConfig
        connection <- liftIO $ connect configRedisConnection
        liftIO $ runRedis connection f
