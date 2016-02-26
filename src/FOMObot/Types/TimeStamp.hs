module FOMObot.Types.TimeStamp where

import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Text (unpack)
import Data.Time.Clock (UTCTime)

import FOMObot.Helpers.Time

newtype TimeStamp = TimeStamp
    { utc :: UTCTime
    } deriving Show

instance FromJSON TimeStamp where
    parseJSON = (TimeStamp <$>) . withText "TimeStamp" (parseTimeStamp . unpack)
