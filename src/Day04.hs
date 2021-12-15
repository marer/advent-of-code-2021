module Day04 (day04) where

import Data.List.Split (chunksOf, splitOn)
import GHC.Base (Semigroup)

day04 :: IO ()
day04 = do
    input <- readFile "data/day04.txt"
    let (boards, numbers) = parseBingo input
    putStrLn $ "Day 4 answer 1: " <> show (answer1 boards numbers)
    putStrLn $ "Day 4 answer 2: " <> show (answer2 boards numbers)

-- common

class Playable a where
    markNumber :: Int -> a -> a
    checkIfWinning :: a -> Bool
    getScore :: a -> Int

type Numbers = [Int]

newtype Board = Board { get :: [(Int, Bool)] }
    deriving (Show)

instance Playable Board where
    markNumber number = Board . map mapItem . get
        where
        mapItem item@(num, _) = if num == number then (num, True) else item
    checkIfWinning board = any (getAll . foldMap (All . snd)) (rows <> columns)
        where
        rows = chunksOf 5 $ get board
        columns = map (`col` rows) [0 .. 4]
    getScore = sum . map fst . filter ((False ==) . snd) . get

col :: Int -> [[b]] -> [b]
col n = map (!! n)

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup All where
    (<>) (All a) (All b) = All (a && b)

instance Monoid All where
    mempty = All True

-- parse

parseBingo :: String -> ([Board], Numbers)
parseBingo input = (boards, numbers)
    where
    inputLines = lines input
    numbers = map read $ splitOn "," $ head inputLines
    boards = map parseBoard $ chunksOf 5 $ filter ("" /=) $ drop 1 inputLines
    parseBoard = Board . map (\s -> (read s, False)) . mconcat . map words

-- part 1

answer1 :: Playable a => [a] -> Numbers -> Int
answer1 [] _ = error "no boards"
answer1 _ [] = error "no winning boards"
answer1 boards (nextNumber : remainingNumbers) =
    case filter checkIfWinning updatedBoards of
        winner : _ -> nextNumber * getScore winner
        [] -> answer1 updatedBoards remainingNumbers
    where
    updatedBoards = map (markNumber nextNumber) boards

-- part 2

answer2 :: Playable a => [a] -> Numbers -> Int
answer2 [] _ = error "no boards"
answer2 _ [] = error "no winning boards"
answer2 boards (nextNumber : remainingNumbers) =
    case (updatedBoards, remainingBoards) of
        ([looser], []) ->  nextNumber * getScore looser
        (_, []) -> error "no definite looser"
        (_, remining) -> answer2 remining remainingNumbers
    where
    updatedBoards = map (markNumber nextNumber) boards
    remainingBoards = filter (not . checkIfWinning) updatedBoards