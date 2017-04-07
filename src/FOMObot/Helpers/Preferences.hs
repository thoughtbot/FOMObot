module FOMObot.Helpers.Preferences
    ( addUserPrefs
    , getUserPrefs
    , deleteUserPrefs
    , removeUserPrefs
    , getUsersForChannel
    ) where

import Control.Lens (view)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Redis as R
import qualified Web.Slack as Slack

import FOMObot.Types.Bot

addUserPrefs :: Slack.UserId -> [String] -> CommandDSL ()
addUserPrefs uid prefs = do
    -- add channel prefs to user
    void $ R.liftRedis $ R.sadd (userPrefsKey uid) $ pack <$> prefs
    -- add user to each channel
    mapM_ (addUserToChannel uid) prefs

getUserPrefs :: Slack.UserId -> Bot [String]
getUserPrefs uid = either (const []) (map unpack)
    <$> (R.liftRedis $ R.smembers $ userPrefsKey uid)

deleteUserPrefs :: Slack.UserId -> Bot ()
deleteUserPrefs uid = do
    -- remove user from each channel
    mapM_ (removeUserFromChannel uid) =<< getUserPrefs uid
    -- delete user prefs
    void $ R.liftRedis $ R.del [userPrefsKey uid]

removeUserPrefs :: Slack.UserId -> [String] -> Bot ()
removeUserPrefs uid prefs = do
    -- remove channel prefs from user
    void $ R.liftRedis $ R.srem (userPrefsKey uid) $ pack <$> prefs
    -- remove user from each channel
    mapM_ (removeUserFromChannel uid) prefs

getUsersForChannel :: String -> Bot [String]
getUsersForChannel cid = either (const []) (map unpack)
    <$> (R.liftRedis $ R.smembers $ channelKey cid)

addUserToChannel :: Slack.UserId -> String -> Bot ()
addUserToChannel uid = void . R.liftRedis . (`R.sadd` [userIdByteString uid]) . channelKey

removeUserFromChannel :: Slack.UserId -> String -> Bot ()
removeUserFromChannel uid = void . R.liftRedis . (`R.srem` [userIdByteString uid]) . channelKey

userPrefsKey :: Slack.UserId -> ByteString
userPrefsKey uid = userKey uid <> ":prefs"

userIdByteString :: Slack.UserId -> ByteString
userIdByteString = encodeUtf8 . (view Slack.getId)

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> userIdByteString uid

channelKey :: String -> ByteString
channelKey cid = "channels:" <> (pack cid) <> ":users"
