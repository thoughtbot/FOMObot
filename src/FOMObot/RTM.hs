module FOMObot.RTM
    ( rtmStartResponse
    ) where

import Control.Lens ((.~), (&))
import Network.Wreq (getWith, asJSON, Response, defaults, param)
import qualified Data.Text as T

import FOMObot.Types.RTMStartResponse

rtmStartResponse :: T.Text -> IO (Response RTMStartResponse)
rtmStartResponse token = asJSON =<< getWith opts "https://slack.com/api/rtm.start"
    where
        opts = defaults & param "token" .~ [token]
