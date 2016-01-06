module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)
import Network.Wreq (responseBody)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import FOMObot.RTM (rtmStartResponse)
import FOMObot.Websockets (runSecureClient)
import FOMObot.Helpers.Bot
import FOMObot.Types.RTMStartResponse
import FOMObot.Types.Bot

runApp :: Bot ()
runApp = do
    connection <- ask
    state <- get

    message <- liftIO $ WS.receiveData connection
    processMessage $ eitherDecode message
    liftIO $ print $ "state: " ++ (show state)
    modify (+1)

initApp :: IO ()
initApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = _url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient uri runApp
