module FOMObot.RTM
    ( RTMStartResponse(..)
    , rtmStartResponse
    ) where

import GHC.Generics (Generic)
import Control.Lens ((.~), (&))
import Data.Aeson (FromJSON)
import Network.Wreq (getWith, asJSON, Response, defaults, param)
import qualified Data.Text as T

data RTMStartResponse = RTMStartResponse { url :: String }
  deriving (Generic, FromJSON, Show)

rtmStartResponse :: T.Text -> IO (Response RTMStartResponse)
rtmStartResponse token = asJSON =<< getWith opts "https://slack.com/api/rtm.start"
    where
        opts = defaults & param "token" .~ [token]
