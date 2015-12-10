import System.Environment
import GHC.Generics
import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Control.Lens
import Data.Aeson
import Network.URI
import Network.Wreq (getWith, asJSON, Response, defaults, param, responseBody)
import Wuss
import qualified Network.WebSockets as WS
import qualified Data.Text as T

app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected!"

    void . forever $ do
        message <- WS.receiveData connection
        print (message :: T.Text)

    WS.sendClose connection (T.pack "Bye!")

data RTMStartResponse = RTMStartResponse { url :: String }
  deriving (Generic, FromJSON, Show)

rtmStartResponse :: T.Text -> IO (Response RTMStartResponse)
rtmStartResponse token = asJSON =<< getWith opts "https://slack.com/api/rtm.start"
    where
        opts = defaults & param "token" .~ [token]

runSecureClient' :: URI -> IO ()
runSecureClient' uri = runSecureClient host 443 (uriPath uri) app
    where
        host = fromJust $ uriRegName <$> uriAuthority uri

main :: IO ()
main = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient' uri
