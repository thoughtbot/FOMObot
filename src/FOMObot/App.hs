module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.State (get, modify)
import Network.Wreq (responseBody)
import qualified Data.Text as T

import FOMObot.RTM (rtmStartResponse)
import FOMObot.Websockets (runSecureClient)
import FOMObot.Helpers.Bot
import FOMObot.Types.RTMStartResponse
import FOMObot.Types.Message
import FOMObot.Types.Bot

runApp :: Bot ()
runApp = do
    state <- get

    message@(Message _ channel _ text) <- receiveMessage
    printMessage message
    sendMessage text channel
    liftIO $ print $ "state: " ++ (show state)
    modify (+1)

initApp :: IO ()
initApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = _url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient uri runApp
