module FOMObot.Types.FOMOChannelReply where

import qualified Data.Text as T

type FOMOChannelReply = T.Text -> IO ()
