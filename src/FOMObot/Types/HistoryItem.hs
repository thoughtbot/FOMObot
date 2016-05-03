module FOMObot.Types.HistoryItem where

import Control.Lens (makeLenses)
import qualified Web.Slack as Slack

data HistoryItem = HistoryItem
    { _historyTimeStamp :: Slack.SlackTimeStamp
    , _historyUserId :: Slack.UserId
    } deriving (Show)

makeLenses ''HistoryItem
