{-# LANGUAGE Rank2Types #-}
module FOMObot.Interpretors.DSL
    ( Bot()
    , safeRunDSL
    ) where

import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (liftIO)
import Data.Lens (uses, view)
import qualified Web.Slack as Slack

import FOMObot.Types.AppState
import FOMObot.Types.BotConfig
import FOMObot.Types.DSL

type Bot = Slack.Slack AppState

safeRunDSL :: (forall a. Free DSL a) -> Bot ()
safeRunDSL = runDSL

runDSL :: Free DSL a -> Bot ()
runDSL (Pure _) = return ()
runDSL (Free End) = return ()

runDSL (Free (GetAppState g)) =
    runDSL . g <$> get

runDSL (Free (SaveChannelState state a)) =
    put state >> runDSL a

runDSL (Free (SendMessage c t a)) =
    Slack.sendMessage c t >> runDSL a

runDSL (Free (RunDatabase d a)) = do
    BotConfig{configRedisConnection} <- uses Slack.userState $ view botConfig
    connection <- liftIO $ connect configRedisConnection
    runDSL =<< liftIO $ runRedis connection f
