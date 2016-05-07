module FOMObot.Helpers.Bot
    ( isFOMOChannel
    , processMessage
    , alertFOMOChannel
    , processCommand
    ) where

import Control.Lens (uses, views, view, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Parsec (parse, manyTill)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, anyChar, char)
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.Algorithm
import FOMObot.Helpers.Redis
import FOMObot.Types.Bot
import FOMObot.Types.ChannelState
import FOMObot.Types.HistoryItem

isFOMOChannel :: Slack.ChannelId -> Bot Bool
isFOMOChannel cid = views Slack.channelId (== cid) <$> getFOMOChannel

getFOMOChannel :: Bot Slack.Channel
getFOMOChannel = do
    channels <- uses Slack.session $ view Slack.slackChannels
    return $ fromJust $ channelFinder channels
  where
    channelFinder = find (views Slack.channelName (== "fomo"))

processMessage :: Slack.Event -> Bot Bool
processMessage (Slack.Message channelID (Slack.UserComment userID) _ messageTimestamp _ _) = do
    config <- getConfig
    let messageChannelID = T.unpack $ channelID ^. Slack.getId

    -- Add the message timestamp to the channel state
    let historyItem = HistoryItem messageTimestamp userID
    channelState <- shiftInHistory config historyItem
        <$> botChannelState messageChannelID

    -- Detect an event that surpasses the threshold
    eventOccurred <- detectFOMOEvent channelState

    -- Save the channel state after adding the event status
    botSaveState messageChannelID
        $ shiftInEvent config eventOccurred channelState

    -- Signal an event only if an event occured and no recent events
    let recentlyNotified = views stateEventHistory or channelState
    return $ eventOccurred && not recentlyNotified

processMessage _ = return False

alertFOMOChannel :: Slack.ChannelId -> Bot ()
alertFOMOChannel channelID = do
    fomoChannel <- view Slack.channelId <$> getFOMOChannel
    Slack.sendMessage fomoChannel message
  where
    message = "Check out <#" <> (channelID ^. Slack.getId) <> ">"

processCommand :: Slack.Event -> Bot ()
processCommand (Slack.Message cid (Slack.UserComment uid) txt _ _ _) =
    case words $ T.unpack txt of
      "add":xs -> addUserPrefs uid $ rights $ (parse parser "") <$> xs
      "remove":xs -> removeUserPrefs uid $ rights $ (parse parser "") <$> xs
      "list":_ -> Slack.sendMessage cid =<< (joinChannels <$> getUserPrefs uid)
      "stop":_ -> deleteUserPrefs uid
      "help":xs -> liftIO $ print xs -- still need to print help instructions
      _ -> return ()
  where
    parser :: Parser String
    parser = (string "<#") *> (manyTill anyChar $ char '>')

    joinChannels [] = "No preferences set."
    joinChannels cids = "<#" <> T.intercalate "> <#" (map T.pack cids) <> ">"

processCommand _ = return ()
