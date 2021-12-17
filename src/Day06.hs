{-# LANGUAGE BangPatterns #-}
module Day06 (day06) where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.Map.Strict (Map, elems, empty, foldlWithKey, insertWith)

day06 :: IO ()
day06 = do
    input <- readFile "data/day06.txt"
    let fishCount = parseFish input
    putStrLn $ "Day 6 answer 1: " <> show (answer1 fishCount 80)
    putStrLn $ "Day 6 answer 2: " <> show (answer1 fishCount 256)

type FishCount = Map Word Int

parseFish :: String -> FishCount
parseFish = foldl' (\acc f -> insertWith (+) f 1 acc) empty . map read . splitOn ","

emptyFc = empty :: FishCount

-- common

answer1 :: FishCount -> Int -> Int
answer1 fishCount days =  sum $ elems $ dayPasses fishCount days
    where
    dayPasses :: FishCount -> Int -> FishCount
    dayPasses !fc 0 = fc
    dayPasses !fc d = dayPasses (foldlWithKey fishMultiply empty fc) (d - 1)
    fishMultiply :: FishCount -> Word -> Int -> FishCount
    fishMultiply acc fish count | fish == 0 =  insertWith (+) 8 count $ insertWith (+) 6 count acc
    fishMultiply acc fish count = insertWith (+) (fish - 1) count acc