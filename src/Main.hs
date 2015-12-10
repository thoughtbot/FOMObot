import System.Environment
import GHC.Generics
import Control.Lens
import Network.Wreq
import Data.Aeson
import qualified Data.Text as T

data RTMStartResponse = RTMStartResponse { url :: String }
  deriving (Generic, FromJSON, Show)

rtmStartResponse :: T.Text -> IO (Response RTMStartResponse)
rtmStartResponse token = asJSON =<< getWith opts "https://slack.com/api/rtm.start"
  where
    opts = defaults & param "token" .~ [token]

main :: IO ()
main = do
  token <- T.pack <$> getEnv "SLACK_API_TOKEN"
  response <- rtmStartResponse token
  print $ response ^. responseBody
