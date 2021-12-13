{-# LANGUAGE RankNTypes #-}
module Day03 (day03) where

import Prelude
import Data.Word (Word16)
import Data.Char (isSpace, intToDigit)
import Data.Bits ((.&.), shiftL, shiftR, complement, zeroBits, bit, testBit)
import Debug.Trace (trace)
import Data.List (intersperse, intercalate)

day03 :: IO ()
day03 = do
    input <- readFile "data/day03.txt"
    let items = parseLine <$> lines input
    putStrLn $ "Day 3 answer 1: " <> show (answer1 items)
    putStrLn $ "Day 3 answer 2: " <> show (answer2 items)

-- parse

type ReportItem = Word16

parseLine :: String -> ReportItem
parseLine = sum . fmap toWord . zip [0..] . reverse . trim
    where
    toWord :: (Int, Char) -> Word16
    toWord (i, chr) = 2^i * (read [chr] :: Word16)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- part 1 - 1131506

commonBitsOf :: (forall a. Ord a => a -> a -> Bool) -> [ReportItem] -> ReportItem
commonBitsOf predicate items = toWord $ foldl countBits initialAcc items
    where
    countBits :: Accumulator -> ReportItem -> Accumulator
    countBits acc number = map (\(bitNo, cnt) -> if testBit number bitNo then (bitNo, cnt + 1) else (bitNo, cnt)) acc
    initialAcc :: Accumulator
    initialAcc = zip [0 .. 15] $ repeat 0
    toWord :: Accumulator -> Word16
    toWord = sum . map bitValue
    bitValue :: (Int, Int) -> Word16
    bitValue (bitNo, cnt) = 2^bitNo * (if predicate (fromIntegral cnt) (fromIntegral (length items) / 2) then 1 else 0)

mostCommonBitsOf :: [ReportItem] -> ReportItem
mostCommonBitsOf = commonBitsOf (>=) 

leastCommonBitsOf :: [ReportItem] -> ReportItem
leastCommonBitsOf = commonBitsOf (<)

type Accumulator =  [(Int, Int)]

answer1 :: [ReportItem] -> Int
answer1 items = fromIntegral gamma * fromIntegral epsilon
    where
    gamma = mostCommonBitsOf items
    epsilon = complement gamma .&. shiftR (complement zeroBits) 4


-- part 2

answer2 :: [ReportItem] -> Int
answer2 items = fromIntegral oxygen * fromIntegral co2
    where
    oxygen = go items mostCommonBitsOf 11
    co2 = go items leastCommonBitsOf 11
    go ::  [ReportItem] -> ([ReportItem] -> ReportItem) -> Int -> ReportItem
    go [] _ _ = 0
    go [i] _ _ = i
    go _ _ c | c < 0 = 0
    go itms patternFn c = go (filter byBit itms) patternFn (c - 1)
        where
        patternBit = testBit (patternFn itms) c
        byBit item = testBit item c == patternBit
