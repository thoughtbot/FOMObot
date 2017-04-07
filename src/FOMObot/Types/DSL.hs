{-# LANGUAGE DeriveFunctor #-}
module FOMObot.Types.DSL
    ( DSL(..)
    , getAppState
    , saveAppState
    , sendMessage
    , runDatabase
    , end
    ) where

import Control.Monad.Free (Free)
import qualified Data.Text as T
import Database.Redis (ConnectInfo, Redis)
import qualified Web.Slack as Slack

import FOMObot.Helpers.Free
import FOMObot.Types.AppState

data DSL a = GetAppState (Slack.SlackState AppState -> a)
           | SaveAppState (Slack.SlackState AppState) a
           | SendMessage Slack.ChannelId T.Text a
           | RunDatabase a
           | End
           deriving Functor

getAppState :: Free DSL (Slack.SlackState AppState)
getAppState = liftFree $ GetAppState id

saveAppState :: Slack.SlackState AppState -> Free DSL ()
saveAppState s = liftFree $ SaveAppState s ()

sendMessage :: Slack.ChannelId -> T.Text -> Free DSL ()
sendMessage c t = liftFree $ SendMessage c t ()

runDatabase :: DatabaseDSL a -> Free DSL a
runDatabase d = liftFree $ RunDatabase d 

end :: Free DSL a
end = liftFree End
