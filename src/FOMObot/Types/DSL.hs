{-# LANGUAGE DeriveFunctor #-}
module FOMObot.Types.DSL
    ( DSL(..)
    , processCommandF
    , getBotConfig
    , getChannelState
    , saveChannelState
    , getEventStatus
    , alert
    , end
    ) where

import Control.Monad.Free (Free)
import qualified Web.Slack as Slack

import FOMObot.Helpers.Free
import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState
import FOMObot.Types.MessageDSL

data DSL a = ProcessCommand Slack.Event a
           | GetBotConfig (BotConfig -> a)
           | GetChannelState Slack.ChannelId (ChannelState -> a)
           | SaveChannelState Slack.ChannelId ChannelState a
           | GetEventStatus Slack.ChannelId (EventStatus -> a)
           | Alert Slack.ChannelId a
           | End
           deriving Functor

processCommandF :: Slack.Event -> Free DSL ()
processCommandF m = liftFree $ ProcessCommand m ()

getBotConfig :: Free DSL BotConfig
getBotConfig = liftFree $ GetBotConfig id

getChannelState :: Slack.ChannelId -> Free DSL ChannelState
getChannelState c = liftFree $ GetChannelState c id

saveChannelState :: Slack.ChannelId -> ChannelState -> Free DSL ()
saveChannelState c s = liftFree $ SaveChannelState c s ()

getEventStatus :: Slack.ChannelId -> Free DSL EventStatus
getEventStatus c = liftFree $ GetEventStatus c id

alert :: Slack.ChannelId -> Free DSL ()
alert c = liftFree $ Alert c ()

end :: Free DSL a
end = liftFree End
