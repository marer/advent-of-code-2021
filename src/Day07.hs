{-# LANGUAGE BangPatterns #-}
module Day07 (day07) where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')

day07 :: IO ()
day07 = do
    input <- readFile "data/day07.txt"
    let crabs = map read $ splitOn "," input :: [Int]
    putStrLn $ "Day 7 answer 1: " <> show (answer1 crabs)
    putStrLn $ "Day 7 answer 2: " <> show (answer2 crabs)


--part 1

answer1 :: [Int] -> Int
answer1 positions = foldl' calculateFuel maxBound [ minPos .. maxPos]
    where
    calculateFuel :: Int -> Int -> Int
    calculateFuel !minSoFar pos = if currentFuel < minSoFar then currentFuel else minSoFar
        where 
        currentFuel = sum $ map (\p -> abs $ pos - p) positions
    minPos = minimum positions
    maxPos = maximum positions

-- part 2

answer2 :: [Int] -> Int
answer2 positions = foldl' calculateFuel maxBound [ minPos .. maxPos]
    where
    calculateFuel :: Int -> Int -> Int
    calculateFuel !minSoFar pos = if currentFuel < minSoFar then currentFuel else minSoFar
        where 
        currentFuel = sum $ map (fuelCost pos) positions
    fuelCost :: Int -> Int -> Int
    fuelCost p1 p2 = round sum
        where        
        sum = (fromIntegral dp / 2) * (1 + fromIntegral dp)
        dp = abs $ p1 - p2

    minPos = minimum positions
    maxPos = maximum positions

