module FOMObot.App
    ( runApp
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Control.Lens ((^.))
import Network.Wreq (responseBody)
import qualified Data.Text as T
import FOMObot.RTM (RTMStartResponse(..), rtmStartResponse)
import FOMObot.Websockets (runSecureClient)

runApp :: IO ()
runApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient uri
