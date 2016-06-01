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

instance Functor DSL where
    fmap f (ProcessCommand m a)     = ProcessCommand m (f a)
    fmap f (GetBotConfig g)         = GetBotConfig (f . g)
    fmap f (GetChannelState c g)    = GetChannelState c (f . g)
    fmap f (SaveChannelState c s a) = SaveChannelState c s (f a)
    fmap f (GetEventStatus c g)     = GetEventStatus c (f . g)
    fmap f (Alert c a)              = Alert c (f a)
    fmap _ End                      = End

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
