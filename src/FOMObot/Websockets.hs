module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Network.URI (URI(..), uriRegName)
import qualified Wuss
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import FOMObot.Types.Bot
import FOMObot.Types.BotConfig

app :: Bot () -> WS.ClientApp ()
app bot connection = do
    putStrLn "Connected!"

    let config = BotConfig connection
    runBot 0 config bot

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> Bot () -> IO ()
runSecureClient uri bot = Wuss.runSecureClient host 443 path $ app bot
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
