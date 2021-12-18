{-# LANGUAGE BangPatterns #-}
module Day08 (day08) where

import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.List (sort, permutations, (\\), elemIndex)
import Data.Maybe (fromJust)


day08 :: IO ()
day08 = do
    input <- readFile "data/day08.txt"
    let entries = parse input
    putStrLn $ "Day 8 answer 1: " <> show (answer1 entries)
    putStrLn $ "Day 8 answer 2: " <> show (answer2 entries)


type Entry = (Sample, Output)
type Sample = [String]
type Output = [String]

parse :: String -> [Entry]
parse = map (toTuple . map words . splitOn " | ") . lines
    where
    toTuple ln = (head ln, ln !! 1)


--part 1

answer1 :: [Entry] -> Int
answer1 = length . filter (flip elem expectedLengths . length). mconcat . map snd
    where
    expectedLengths = [2, 3, 4, 7] :: [Int]

-- part 2

answer2 :: [Entry] -> Int
answer2 = sum . map decode

correctPattern :: [Char]
correctPattern = "abcdefg"

decode :: Entry -> Int
decode (sample, output) = toNumber $ map (toDigit . mapPattern) output
    where
    patt = findPattern sample

    mapPattern :: String -> String
    mapPattern = map ((correctPattern !!) . fromJust . flip elemIndex patt)
        

    toDigit :: String -> Int
    toDigit s | s `eql` "abcefg" = 0
    toDigit s | s `eql` "cf" = 1
    toDigit s | s `eql` "acdeg" = 2
    toDigit s | s `eql` "acdfg" = 3
    toDigit s | s `eql` "bcdf" = 4
    toDigit s | s `eql` "abdfg" = 5
    toDigit s | s `eql` "abdefg" = 6
    toDigit s | s `eql` "acf" = 7
    toDigit s | s `eql` "abcdefg" = 8
    toDigit s | s `eql` "abcdfg" = 9
    toDigit s = error $ "wrong number pattern: " <> s

    toNumber :: [Int] -> Int
    toNumber = sum . zipWith (\i d -> 10^i * d) [0 .. ] . reverse


findPattern :: Sample -> String
findPattern sample = head $ filter checkPattern $ permutations correctPattern
    where
    checkPattern :: String -> Bool
    checkPattern [a, b, c, d, e, f, g] = all check sample
        where
        check :: String -> Bool
        check s | length s == 2 = s `eql` [c, f]
        check s | length s == 3 = s `eql` [a, c, f]
        check s | length s == 4 = s `eql` [b, c, d, f]
        check s | length s == 5 = s `eql` [a, c, d, e, g]
                               || s `eql` [a, c, d, f, g]
                               || s `eql` [a, b, d, f, g]
        check s | length s == 6 = s `eql` [a, b, c, e, f, g]
                               || s `eql` [a, b, d, e, f, g]
                               || s `eql` [a, b, c, d, f, g]
        check s | length s == 7 = s `eql` [a, b, c, d, e, f, g]
        check _ = False
    checkPattern _ = error "should never happen"

eql :: (Eq a) => [a] -> [a] -> Bool
eql x y = null (x \\ y) && null (y \\ x)