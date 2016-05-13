module FOMObot.Helpers.DMChannel
    ( setDMChannel
    , isDMChannel
    ) where

import Control.Lens ((^.), views)
import Control.Monad (void)
import Data.ByteString (ByteString)
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

dmChannelKey :: Slack.UserId -> ByteString
dmChannelKey uid = userKey uid <> ":channel"

userKey :: Slack.UserId -> ByteString
userKey uid = "users:" <> (encodeUtf8 $ uid ^. Slack.getId)
