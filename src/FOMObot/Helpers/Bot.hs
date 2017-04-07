module FOMObot.Helpers.Bot
    ( getConfig
    , getChannelState
    , saveChannelState
    ) where

import Control.Lens (view, over, (^.))
import qualified Data.HashMap as HM
import qualified Data.Text as T
import qualified Web.Slack as Slack

import FOMObot.Types.AppState
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState
import FOMObot.Types.ChannelState
import FOMObot.Types.DSL

getConfig :: Free DSL BotConfig
getConfig = view (Slack.userState . botConfig) <$> getAppState

getState :: Free DSL BotState
getState = view (Slack.userState . botState) <$> getAppState

modifyState :: (BotState -> BotState) -> Free DSL ()
modifyState f = saveAppState =<<
    over (Slack.userState . botState) f <$> getAppState

getChannelState :: Slack.ChannelId -> Free DSL ChannelState
getChannelState cid = do
    mChannelState <- HM.lookup channelId <$> getState
    maybe (createChannelState cid) return mChannelState
  where
    channelId = T.unpack $ cid ^. Slack.getId

createChannelState :: Slack.ChannelId -> Free DSL ChannelState
createChannelState cid = do
    let newChannelState = ChannelState [] []
    botSaveState cid newChannelState
    return newChannelState

saveChannelState :: Slack.ChannelId -> ChannelState -> Free DSL ()
saveChannelState cid = modifyState . (HM.insert channelId)
  where
    channelId = T.unpack $ cid ^. Slack.getId
