module FOMObot.Helpers.Algorithm
    ( Density
    , shiftIn
    , isArrayFull
    ) where

type Density = Double

shiftIn :: Int -> a -> [a] -> [a]
shiftIn size item xs
    | isArrayFull xs size = item:init xs
    | otherwise = item:xs

isArrayFull :: [a] -> Int -> Bool
isArrayFull xs size = length xs == size
