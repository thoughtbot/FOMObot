module FOMObot.Types.MessageDSL
    ( MessageDSL(..)
    , Density
    , EventStatus
    , shiftInHistory
    , shiftInEvent
    , calcDensity
    , detectEvent
    ) where

import Control.Monad.Free (Free)

import FOMObot.Helpers.Free
import FOMObot.Types.HistoryItem

type Density = Double
type EventStatus = Bool

data MessageDSL a = ShiftInHistory HistoryItem a
                  | ShiftInEvent EventStatus a
                  | CalcDensity (Density -> a)
                  | DetectEvent Density (EventStatus -> a)

instance Functor MessageDSL where
    fmap f (ShiftInHistory h a) = ShiftInHistory h (f a)
    fmap f (ShiftInEvent e a)   = ShiftInEvent e (f a)
    fmap f (CalcDensity g)      = CalcDensity (f . g)
    fmap f (DetectEvent d g)    = DetectEvent d (f . g)

shiftInHistory :: HistoryItem -> Free MessageDSL ()
shiftInHistory h = liftFree $ ShiftInHistory h ()

shiftInEvent :: EventStatus -> Free MessageDSL ()
shiftInEvent e = liftFree $ ShiftInEvent e ()

calcDensity :: Free MessageDSL Density
calcDensity = liftFree $ CalcDensity id

detectEvent :: Density -> Free MessageDSL EventStatus
detectEvent d = liftFree $ DetectEvent d id
