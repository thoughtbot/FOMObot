module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Control.Lens (uses, views, (^.))
import Control.Monad (void, when, unless)
import qualified Web.Slack as Slack

import FOMObot.Helpers.Bot
import FOMObot.Helpers.DMChannel
import FOMObot.Types.AppState
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig
import FOMObot.Types.BotState

runApp :: Slack.Event -> Bot ()
runApp m@(Slack.Message cid (Slack.UserComment uid) _ _ _ _) = do
    ignoreFOMOChannel <- isFOMOChannel cid
    unless ignoreFOMOChannel $ do
        isDM <- isDMChannel uid cid
        if isDM
          then processCommand m
          else (`when` alertFOMOChannel cid) =<< processMessage m

runApp (Slack.ImCreated uid (Slack.IM cid _ _ _ _ _)) = setDMChannel uid cid

runApp Slack.Hello = do
    ims <- uses Slack.session (views Slack.slackIms $ map pullOutUserAndChannel)
    mapM_ (uncurry setDMChannel) ims
  where
    pullOutUserAndChannel im = (im ^. Slack.imUser, im ^. Slack.imId)

runApp _ = return ()

initApp :: IO ()
initApp = do
    token <- getEnv "SLACK_API_TOKEN"
    config <- buildConfig
    void $ Slack.runBot (Slack.SlackConfig token) runApp $ AppState config emptyState
