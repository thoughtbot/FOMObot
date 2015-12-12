module FOMObot.Types.Bot where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import qualified Network.WebSockets as WS

type Bot = ReaderT WS.Connection (StateT Int IO)
