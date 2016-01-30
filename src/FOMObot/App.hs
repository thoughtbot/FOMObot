module FOMObot.App
    ( initApp
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
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
    printBot message
    updateState message
    alertFOMOChannel messageText
    state <- get
    printBot $ "state: " ++ (show state)

initApp :: IO ()
initApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    RTMStartResponse{..} <- rtmStartResponse token
    longAlpha <- read <$> getEnv "LONG_ALPHA"
    shortAlpha <- read <$> getEnv "SHORT_ALPHA"
    let partialConfig = BotConfig (getFOMOChannelID responseChannels) responseSelfID longAlpha shortAlpha
    let uri = fromJust $ parseURI responseURL
    runSecureClient uri partialConfig runApp
    where
        getFOMOChannelID = channelID . fromJust . (find isFOMOChannel)
        isFOMOChannel = (== "fomo") . channelName
