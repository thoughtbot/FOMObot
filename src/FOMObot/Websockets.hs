module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Network.URI (URI(..), uriRegName)
import qualified Wuss (runSecureClient)
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import FOMObot.MessageParser (parseMessage)

app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected!"

    void . forever $ do
        message <- WS.receiveData connection
        let msg = parseMessage message
        print msg

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> IO ()
runSecureClient uri = Wuss.runSecureClient host 443 path app
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
