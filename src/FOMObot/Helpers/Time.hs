module FOMObot.Helpers.Time
    ( parseTimeStamp
    ) where

import Data.Time (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

parseTimeStamp :: (Monad m) => String -> m UTCTime
parseTimeStamp = parseTimeM True defaultTimeLocale "%s%Q"
