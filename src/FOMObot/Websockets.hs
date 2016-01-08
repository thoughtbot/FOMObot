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

app :: PartialConfig -> Bot () -> WS.ClientApp ()
app partialConfig bot connection = do
    putStrLn "Connected!"

    let config = partialConfig connection
    runBot 0 config bot

    WS.sendClose connection $ T.pack "Bye!"

runSecureClient :: URI -> PartialConfig -> Bot () -> IO ()
runSecureClient uri partialConfig bot = Wuss.runSecureClient host 443 path clientApp
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
        clientApp = app partialConfig bot
