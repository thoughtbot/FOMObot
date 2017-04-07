{-# LANGUAGE DeriveFunctor #-}
module FOMObot.Types.CommandDSL
    ( CommandDSL(..)
    , getPreferences
    , savePreferences
    , getDMChannel
    , saveDMChannel
    ) where

import Control.Monad.Free (Free)
import qualified Data.Text as T

import FOMObot.Helpers.Free

data CommandDSL a = GetPreferences String ([String] -> a)
                  | SavePreferences String [String] a
                  | GetDMChannel String (String -> a)
                  | SaveDMChannel String String a
                  deriving Functor

getPreferences :: String -> Free CommandDSL [String]
getPreferences u = liftFree $ GetPreferences u id

savePreferences :: String -> [String] -> Free CommandDSL ()
savePreferences u cs = liftFree $ SavePreferences u cs ()

getDMChannel :: String -> Free CommandDSL String
getDMChannel u = liftFree $ GetDMChannel u id

saveDMChannel :: String -> String -> Free CommandDSL ()
saveDMChannel u c = liftFree $ SaveDMChannel u c ()
