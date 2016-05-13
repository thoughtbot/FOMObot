module FOMObot.Helpers.CommandProcessor
    ( processCommand
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.Preferences
import FOMObot.Types.Bot
import FOMObot.Types.Command

processCommand :: Slack.Event -> Bot ()
processCommand (Slack.Message cid (Slack.UserComment uid) txt _ _ _) =
    case parseCommand $ T.unpack txt of
      (Add xs) -> addUserPrefs uid xs
      (Remove xs) -> removeUserPrefs uid xs
      List -> Slack.sendMessage cid =<< (joinChannels <$> getUserPrefs uid)
      Stop -> deleteUserPrefs uid
      Help -> liftIO $ print "help" -- still need to print help instructions
      Unknown -> return ()
  where
    joinChannels [] = "No preferences set."
    joinChannels cids = "<#" <> T.intercalate "> <#" (map T.pack cids) <> ">"

processCommand _ = return ()
