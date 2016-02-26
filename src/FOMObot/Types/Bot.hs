module FOMObot.Types.Bot where

import Control.Monad (forever, void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Control.Monad.Trans (liftIO)
import qualified Data.HashMap as HM (Map, lookup, empty, insert)
import qualified Network.WebSockets as WS

import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState

type BotState = HM.Map String ChannelState

type Bot = ReaderT BotConfig (StateT BotState IO)

runBot :: BotState -> BotConfig -> Bot () -> IO ()
runBot initialState config bot = void $ execInitialState $ runReaderT foreverBot config
    where
        foreverBot = forever bot
        execInitialState = (`execStateT` initialState)

emptyState :: BotState
emptyState = HM.empty

botChannelState :: String -> Bot ChannelState
botChannelState channelID = do
    mChannelState <- gets $ HM.lookup channelID
    maybe (botInsert channelID) return mChannelState

botInsert :: String -> Bot ChannelState
botInsert channelID = do
    let newChannelState = ChannelState [] []
    modify $ HM.insert channelID newChannelState
    return newChannelState

botSaveState :: String -> ChannelState -> Bot ()
botSaveState channelID = modify . (HM.insert channelID)

printBot :: Show a => a -> Bot ()
printBot = liftIO . print

botConnection :: Bot WS.Connection
botConnection = configConnection <$> ask
