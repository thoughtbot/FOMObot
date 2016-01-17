module FOMObot.Helpers.MovingAverage
    ( singleExpSmoothing
    ) where

singleExpSmoothing :: Num a => a -> a -> a -> a
singleExpSmoothing alpha avg datum = (1 - alpha) * avg + alpha * datum
