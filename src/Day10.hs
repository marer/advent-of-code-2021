
module Day10 (day10) where

import Data.List (sort)

day10 :: IO ()
day10 = do
    input <- readFile "data/day10.txt"
    let lns = lines input
    putStrLn $ "Day 10 answer 1: " <> show (answer1 lns)
    putStrLn $ "Day 10 answer 2: " <> show (answer2 lns)

data ParseResult
    = Correct
    | Corrupt Char
    | Incomplete String

parseLine :: String -> String -> ParseResult
parseLine [] [] = Correct
parseLine stack [] = Incomplete stack
parseLine [] (c : rest) | c `elem` ">}])" = Corrupt c
parseLine stack (c : rest) | c `elem` "([{<" = parseLine (c : stack) rest
parseLine (s : srest) (c : rest) = case (s, c) of
    ('(', ')') -> parseLine srest rest
    ('[', ']') -> parseLine srest rest
    ('{', '}') -> parseLine srest rest
    ('<', '>') -> parseLine srest rest
    _ -> Corrupt c
parseLine _ _ = Correct

-- part 1

answer1 :: [String] -> Int
answer1 = sum . map (pointsOf . parseLine "")
    where
    pointsOf :: ParseResult -> Int
    pointsOf (Corrupt ')') = 3
    pointsOf (Corrupt ']') = 57
    pointsOf (Corrupt '}') = 1197
    pointsOf (Corrupt '>') = 25137
    pointsOf _ = 0

-- part 2

answer2 :: [String] -> Int
answer2 = pickMiddleScore . sort . map (toScore . fromIncomplete) . filter isIncomplete . map (parseLine "")
    where
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False

    fromIncomplete (Incomplete str) = str
    fromIncomplete _ = ""

    pickMiddleScore :: [Int] -> Int
    pickMiddleScore [] = 0
    pickMiddleScore scores = scores !! middle
        where 
        middle = length scores `div` 2

    toScore :: String  -> Int
    toScore = foldl (\acc c -> 5 * acc + pointsOf c) 0

    pointsOf :: Char -> Int
    pointsOf '(' = 1
    pointsOf '[' = 2
    pointsOf '{' = 3
    pointsOf '<' = 4
    pointsOf _ = 0