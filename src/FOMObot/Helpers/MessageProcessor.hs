module FOMObot.Helpers.MessageProcessor
    ( processMessage
    ) where

import qualified Web.Slack as Slack
import Control.Monad.Free (Free)
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)

import FOMObot.Types.DSL
import FOMObot.Types.MessageDSL
import FOMObot.Types.HistoryItem

processMessage :: Slack.Event -> Free DSL ()
processMessage (Slack.Message cid (Slack.UserComment userId) _ timestamp _ _) = do
    config <- getBotConfig
    channelState <- getChannelState cid
    let historyItem = HistoryItem timestamp userId
    (_, newState) <- runStateT (runReaderT (runMessageDSL $ do
        shiftInHistory historyItem
        event <- detectEvent =<< calcDensity
        shiftInEvent event
        return event) config) channelState
    saveChannelState cid newState

processMessage _ = return ()
