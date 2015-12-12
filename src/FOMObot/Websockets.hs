module FOMObot.Websockets
    ( runSecureClient
    ) where

import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execStateT)
import Network.URI (URI(..), uriRegName)
import qualified Wuss
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import FOMObot.Types.Bot

app :: Bot () -> WS.ClientApp ()
app bot connection = do
    putStrLn "Connected!"

    void $ execInitialState $ runReaderT foreverBot connection
    WS.sendClose connection $ T.pack "Bye!"
    where
        foreverBot = forever bot
        execInitialState = (`execStateT` 0)

runSecureClient :: URI -> Bot () -> IO ()
runSecureClient uri bot = Wuss.runSecureClient host 443 path $ app bot
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri
