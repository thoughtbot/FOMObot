module FOMObot.Helpers.Preferences
    ( addUserPrefs
    , getUserPrefs
    , deleteUserPrefs
    , removeUserPrefs
    ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Redis as R
import qualified Web.Slack as Slack

import FOMObot.Types.Bot

addUserPrefs :: Slack.UserId -> [String] -> Bot ()
addUserPrefs uid prefs = void $ R.liftRedis $ R.sadd (userPrefsKey uid) $ pack <$> prefs

getUserPrefs :: Slack.UserId -> Bot [String]
getUserPrefs uid = either (const []) (map unpack)
    <$> (R.liftRedis $ R.smembers $ userPrefsKey uid)

deleteUserPrefs :: Slack.UserId -> Bot ()
deleteUserPrefs uid = void $ R.liftRedis $ R.del [userPrefsKey uid]

removeUserPrefs :: Slack.UserId -> [String] -> Bot ()
removeUserPrefs uid prefs = void $ R.liftRedis $ R.srem (userPrefsKey uid) $ pack <$> prefs

userPrefsKey :: Slack.UserId -> ByteString
userPrefsKey uid = userKey uid <> ":prefs"

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> (encodeUtf8 $ uid ^. Slack.getId)
