module FOMObot.Helpers.MessageProcessor
    ( processMessage
    ) where

import qualified Web.Slack as Slack
import Control.Monad.Free (Free)
import Control.Monad.State (execStateT)

import FOMObot.Interpretors.MessageDSL
import FOMObot.Helpers.Bot
import FOMObot.Types.DSL
import FOMObot.Types.MessageDSL
import FOMObot.Types.HistoryItem

processMessage :: Slack.Event -> Free DSL ()
processMessage (Slack.Message cid (Slack.UserComment userId) _ timestamp _ _) = do
    channelState <- getChannelState cid
    let historyItem = HistoryItem timestamp userId

    let program = runMessageDSL $ do
        shiftInHistory historyItem
        event <- detectEvent =<< calcDensity
        shiftInEvent event
        return event

    newState <- execStateT program channelState
    saveChannelState cid newState

processMessage _ = end
