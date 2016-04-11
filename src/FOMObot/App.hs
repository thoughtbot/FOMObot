module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Control.Monad (void, when)
import qualified Web.Slack as Slack

import FOMObot.Helpers.Bot
import FOMObot.Types.AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState

runApp :: Slack.Event -> Bot ()
runApp m@(Slack.Message cid (Slack.UserComment _) _ _ _ _) = do
    ignoreFOMOChannel <- isFOMOChannel cid
    if ignoreFOMOChannel
       then return ()
       else (`when` alertFOMOChannel cid) =<< processMessage m

runApp _ = return ()

initApp :: IO ()
initApp = do
    token <- getEnv "SLACK_API_TOKEN"
    config <- buildConfig
    void $ Slack.runBot (Slack.SlackConfig token) runApp $ AppState config emptyState
