module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Network.URI (URI(..), uriRegName)
import Data.Aeson (eitherDecode)
import qualified Wuss
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import FOMObot.Types.MessageProcessor

app :: MessageProcessor -> WS.ClientApp ()
app processor connection = do
    putStrLn "Connected!"

    void . forever $ do
        message <- WS.receiveData connection
        processor $ eitherDecode message

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> MessageProcessor -> IO ()
runSecureClient uri processor = Wuss.runSecureClient host 443 path $ app processor
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
