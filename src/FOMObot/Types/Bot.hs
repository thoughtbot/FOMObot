{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FOMObot.Types.Bot where

import Control.Lens (view, uses, modifying, set)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap as HM
import Database.Redis (MonadRedis(..), runRedis, connect)
import qualified Web.Slack as Slack

import FOMObot.Types.AppState
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState
import FOMObot.Types.ChannelState

type Bot = Slack.Slack AppState

instance MonadRedis Bot where
    liftRedis f = do
        BotConfig{configRedisConnection} <- getConfig
        connection <- liftIO $ connect configRedisConnection
        liftIO $ runRedis connection f

getConfig :: Bot BotConfig
getConfig = uses Slack.userState $ view botConfig

getState :: Bot BotState
getState = uses Slack.userState $ view botState

modifyState :: (BotState -> BotState) -> Bot ()
modifyState f = do
    state <- getState
    modifying Slack.userState $ set botState $ f state

botChannelState :: String -> Bot ChannelState
botChannelState channelID = do
    mChannelState <- HM.lookup channelID <$> getState
    maybe (botInsert channelID) return mChannelState

botInsert :: String -> Bot ChannelState
botInsert channelID = do
    let newChannelState = ChannelState [] []
    botSaveState channelID newChannelState
    return newChannelState

botSaveState :: String -> ChannelState -> Bot ()
botSaveState channelID = modifyState . (HM.insert channelID)
