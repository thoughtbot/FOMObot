module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Network.URI (URI(..), uriRegName)
import qualified Wuss (runSecureClient)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected!"

    void . forever $ do
        message <- WS.receiveData connection
        T.putStrLn message

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> IO ()
runSecureClient uri = Wuss.runSecureClient host 443 path app
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
