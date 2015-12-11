module FOMObot.Types.RTMStartResponse where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data RTMStartResponse = RTMStartResponse { url :: String }
  deriving (Generic, FromJSON, Show)
