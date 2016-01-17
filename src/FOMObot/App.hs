module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (get)
import qualified Data.Text as T
import Data.List (find)

import FOMObot.RTM (rtmStartResponse)
import FOMObot.Websockets (runSecureClient)
import FOMObot.Helpers.Bot
import FOMObot.Types.RTMStartResponse
import FOMObot.Types.Channel
import FOMObot.Types.Message
import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

runApp :: Bot ()
runApp = do
    message@Message{..} <- receiveMessage
    printMessage message
    updateState message
    alertFOMOChannel _text
    state <- get
    liftIO $ print $ "state: " ++ (show state)

initApp :: IO ()
initApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    longAlpha <- read <$> getEnv "LONG_ALPHA"
    shortAlpha <- read <$> getEnv "SHORT_ALPHA"
    let partialConfig = BotConfig (getFOMOChannelID response) (_selfID response) longAlpha shortAlpha
    let uri = fromJust $ parseURI $ _url response
    runSecureClient uri partialConfig runApp
    where
        getFOMOChannelID = _id . fromJust . (find isFOMOChannel) . _channels
        isFOMOChannel = (== "fomo") . _name
