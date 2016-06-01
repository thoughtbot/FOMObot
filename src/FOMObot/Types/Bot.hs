{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FOMObot.Types.Bot
    ( Bot()
    , getConfig
    , botChannelState
    , botSaveState
    ) where

import Control.Lens (view, uses, modifying, set, (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap as HM
import qualified Data.Text as T
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

botChannelState :: Slack.ChannelId -> Bot ChannelState
botChannelState cid = do
    mChannelState <- HM.lookup channelId <$> getState
    maybe (botInsert cid) return mChannelState
  where
    channelId = T.unpack $ cid ^. Slack.getId

botInsert :: Slack.ChannelId -> Bot ChannelState
botInsert cid = do
    let newChannelState = ChannelState [] []
    botSaveState cid newChannelState
    return newChannelState

botSaveState :: Slack.ChannelId -> ChannelState -> Bot ()
botSaveState cid = modifyState . (HM.insert channelId)
  where
    channelId = T.unpack $ cid ^. Slack.getId
