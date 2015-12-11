module FOMObot.App
    ( runApp
    , messageProcessor
    ) where

import System.Environment (getEnv)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Control.Lens ((^.))
import Network.Wreq (responseBody)
import qualified Data.Text as T

import FOMObot.RTM (rtmStartResponse)
import FOMObot.Websockets (runSecureClient)
import FOMObot.Types.RTMStartResponse
import FOMObot.Types.Message
import FOMObot.Types.MessageProcessor

messageProcessor :: MessageProcessor
messageProcessor = either doNothing printMessage
    where
        doNothing = const $ return ()

printMessage :: Message -> IO ()
printMessage m@(Message t _ _ _)
    | t == "message" = print m
    | otherwise = return ()

runApp :: IO ()
runApp = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = _url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient uri messageProcessor
