module FOMObot.Types.MessageProcessor where

import FOMObot.Types.Message

type MessageProcessor = Either String Message -> IO ()
