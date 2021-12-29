module Day14 (day14) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (foldl')

day14 :: IO ()
day14 = do
    input <- readFile "data/day14.txt"
    let (template, rules) = parse input
    putStrLn $ "Day 14 answer 1: " <> show (answer1 template rules)
    putStrLn $ "Day 14 answer 2: " <> show (answer2 template rules)


--parse

type Template = String
type Pair = (Char, Char)
type Rules = Map Pair  Char

parse :: String -> (Template, Rules)
parse str = (head lns, rules)
    where
    lns = lines str
    rules = Map.fromList $ map parseRule $ drop 2 lns
    parseRule :: String -> (Pair, Char)
    parseRule ln = case splitOn " -> " ln of
        [pair, output] -> ((head pair, pair !! 1), head output)
        _ -> error $ "invalid rule: " <> ln


-- common

toFrequency :: Template -> Map Char Int
toFrequency = foldl' (\acc c -> Map.insertWith (+) c 1 acc) Map.empty

-- part 1

answer1 :: Template -> Rules -> Int
answer1 template rules = maximum elementFreq - minimum elementFreq
    where
    elementFreq = foldl' (\acc c -> Map.insertWith (+) c 1 acc) Map.empty polymer
    polymer = processPolymer template 10
    
    processPolymer :: Template -> Int -> Template
    processPolymer polymer 0 = polymer
    processPolymer polymer stepsLeft = processPolymer newPolymer (stepsLeft - 1)
        where
        newPolymer = head polymer : concat (zipWith (curry insert) polymer (drop 1 polymer))

    insert :: Pair -> String
    insert pair = [middle, snd pair]
        where
        middle = case Map.lookup pair rules of
            Just c -> c
            _ -> error $ "invalid pair: " <> show pair

-- part 2

answer2 :: Template -> Rules -> Int
answer2 template rules = ceiling ( fromIntegral (maximum elementFreq - minimum elementFreq) / 2)
    where
    steps = 40

    elementFreq :: Map Char Int
    elementFreq = Map.foldrWithKey foldFreq Map.empty finalPairs
        
    foldFreq :: Pair -> Int -> Map Char Int -> Map Char Int
    foldFreq (f, s) count =  Map.insertWith (+) s count . Map.insertWith (+) f count

    finalPairs = processStep initialPairs steps

    initialPairs :: Map Pair Int
    initialPairs = foldl' (\acc p -> Map.insertWith (+) p 1 acc) Map.empty $ zip template (drop 1 template)

    processStep :: Map Pair Int -> Int -> Map Pair Int
    processStep pairs 0 = pairs
    processStep pairs stepsLeft = processStep newPairs (stepsLeft - 1)
        where
        newPairs = Map.foldrWithKey foldPairs Map.empty pairs
    
    foldPairs :: Pair -> Int -> Map Pair Int -> Map Pair Int
    foldPairs pair@(f, s) count = Map.insertWith (+) (middle, s) count . Map.insertWith (+) (f, middle) count
        where
        middle = case Map.lookup pair rules of
            Just c -> c
            _ -> error $ "invalid pair: " <> show pair
       
