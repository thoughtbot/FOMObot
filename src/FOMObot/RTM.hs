module FOMObot.RTM
    ( rtmStartResponse
    ) where

import Control.Lens ((.~), (&), (^.))
import Network.Wreq (getWith, asJSON, defaults, param, responseBody)
import qualified Data.Text as T

import FOMObot.Types.RTMStartResponse

rtmStartResponse :: T.Text -> IO RTMStartResponse
rtmStartResponse token = getResponseBody <$> (asJSON =<< request)
    where
        request = getWith opts "https://slack.com/api/rtm.start"
        opts = defaults & param "token" .~ [token]
        getResponseBody = (^. responseBody)
