module FOMObot.Types.Bot where

import Control.Monad (forever, void)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import qualified Network.WebSockets as WS

type Bot = ReaderT WS.Connection (StateT Int IO)

runBot :: Int -> WS.Connection -> Bot () -> IO ()
runBot initialState connection bot = void $ execInitialState $ runReaderT foreverBot connection
    where
        foreverBot = forever bot
        execInitialState = (`execStateT` initialState)
