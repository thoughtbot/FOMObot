module FOMObot.Helpers.Time
    ( diffTime
    )where

import Data.Maybe (fromJust)
import Data.Time (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (diffUTCTime, NominalDiffTime)

diffTime :: String -> String -> NominalDiffTime
diffTime ts1 ts2 = fromJust $ diffUTCTime <$> (parse ts1) <*> (parse ts2)
    where
        parse = parseTimeM True defaultTimeLocale "%s%Q"
