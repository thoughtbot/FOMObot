module FOMObot.Types.Bot where

import Control.Monad (forever, void)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import Data.HashMap (Map)

import FOMObot.Types.BotConfig
import FOMObot.Types.ChannelState

type BotState = Map String ChannelState

type Bot = ReaderT BotConfig (StateT BotState IO)

runBot :: BotState -> BotConfig -> Bot () -> IO ()
runBot initialState config bot = void $ execInitialState $ runReaderT foreverBot config
    where
        foreverBot = forever bot
        execInitialState = (`execStateT` initialState)
