import System.Environment (getEnv)
import GHC.Generics (Generic)
import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Control.Lens ((.~), (&), (^.))
import Data.Aeson (FromJSON)
import Network.URI (URI(..), parseURI, uriRegName)
import Network.Wreq (getWith, asJSON, Response, defaults, param, responseBody)
import Wuss (runSecureClient)
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

data RTMStartResponse = RTMStartResponse { url :: String }
  deriving (Generic, FromJSON, Show)

rtmStartResponse :: T.Text -> IO (Response RTMStartResponse)
rtmStartResponse token = asJSON =<< getWith opts "https://slack.com/api/rtm.start"
    where
        opts = defaults & param "token" .~ [token]

runSecureClient' :: URI -> IO ()
runSecureClient' uri = runSecureClient host 443 path app
    where
        host = fromJust $ uriRegName <$> uriAuthority uri
        path = uriPath uri

main :: IO ()
main = do
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    response <- rtmStartResponse token
    let socketURL = url $ response ^. responseBody
    let uri = fromJust $ parseURI socketURL
    runSecureClient' uri
