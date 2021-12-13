module Day01 (day01) where

import Prelude

day01 :: IO ()
day01 = do
    input <- readFile "data/day01.txt"
    let depths = read <$> lines input :: Depths
    putStrLn $ "Day 1 answer 1: " <> show (process1 depths)
    putStrLn $ "Day 1 answer 2: " <> show (process2 depths)

process1 :: Depths -> Int
process1 = countIncreases . toChange

process2 :: Depths -> Int
process2 depths = countIncreases $ toChange averages
    where 
    averages = sum <$> zip3 depths (drop 1 depths) (drop 2 depths)
    sum (f, s, t) = f + s + t

toChange :: Depths -> DepthChanges
toChange depths = zip depths (drop 1 depths)

countIncreases :: DepthChanges -> Int
countIncreases = length . filter (\(f, s) -> s > f)

type DepthChanges = [(Int, Int)]

type Depths = [Int]

