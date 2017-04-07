module FOMObot.Interpretors.CommandDSL
    ( runCommandDSL
    ) where

import Control.Monad.Free (Free(..))

import FOMObot.Types.CommandDSL
import FOMObot.Types.DSL

runCommandDSL :: Free CommandDSL a -> Free DSL a
runCommandDSL (Pure _) = return ()

runCommandDSL (Free (GetPreferences uid g)) = do
    prefs <- 
    either (const []) (map unpack)
    <$> (R.liftRedis $ R.smembers $ userPrefsKey uid)
addUserPrefs uid channels
runCommandDSL a

runCommandDSL (Free (Remove uid channels a)) = do
removeUserPrefs uid channels
runCommandDSL a

runCommandDSL (Free (List cid uid a)) = do
prefs <- getUserPrefs uid
sendMessage cid $ joinChannels prefs
runCommandDSL a

runCommandDSL (Free (Stop uid a)) = do
deleteUserPrefs uid
runCommandDSL a

runCommandDSL (Free (Help cid a)) = do
sendMessage cid helpText
runCommandDSL a

joinChannels :: [String] -> Text
joinChannels [] = "No preferences set."
joinChannels cids = "<#" <> T.intercalate "> <#" (map T.pack cids) <> ">"
