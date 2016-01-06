module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Network.URI (URI(..), uriRegName)
import qualified Wuss
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import FOMObot.Types.Bot

app :: Bot () -> WS.ClientApp ()
app bot connection = do
    putStrLn "Connected!"

    runBot 0 connection bot

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> Bot () -> IO ()
runSecureClient uri bot = Wuss.runSecureClient host 443 path $ app bot
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
