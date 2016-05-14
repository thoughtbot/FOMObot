module FOMObot.Helpers.DMChannel
    ( setDMChannel
    , isDMChannel
    , getDMChannel
    ) where

import Control.Lens ((^.), views)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Redis as R
import qualified Web.Slack as Slack

import FOMObot.Types.Bot

setDMChannel :: Slack.UserId -> Slack.IMId -> Bot ()
setDMChannel uid cid = void $ R.liftRedis $ R.set (dmChannelKey uid) dmChannelValue
  where
    dmChannelValue = encodeUtf8 $ cid ^. Slack.getId

isDMChannel :: Slack.UserId -> Slack.ChannelId -> Bot Bool
isDMChannel uid cid = do
    channelId <- R.liftRedis $ R.get (dmChannelKey uid)
    return $ channelId == views Slack.getId (Right . Just . encodeUtf8) cid

getDMChannel :: String -> Bot (Maybe String)
getDMChannel uid = either (const Nothing) (maybe Nothing (Just . unpack))
    <$> (R.liftRedis $ R.get (pack uid <> ":channel"))

dmChannelKey :: Slack.UserId -> ByteString
dmChannelKey uid = userKey uid <> ":channel"

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> (encodeUtf8 $ uid ^. Slack.getId)
