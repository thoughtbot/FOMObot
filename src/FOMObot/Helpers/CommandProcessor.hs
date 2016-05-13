module FOMObot.Helpers.CommandProcessor
    ( processCommand
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Parsec (parse, manyTill)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, anyChar, char)
import qualified Web.Slack as Slack
import qualified Web.Slack.Message as Slack

import FOMObot.Helpers.Preferences
import FOMObot.Types.Bot

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
