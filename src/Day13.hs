module Day13 (day13) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (isPrefixOf, foldl')
import GHC.Exts (groupWith)
import Data.List.Split (splitOn, chunksOf)

day13 :: IO ()
day13 = do
    input <- readFile "data/day13.txt"
    let (points, folds) = parse input
    putStrLn $ "Day 13 answer 1: " <> show (answer1 points folds)
    putStrLn $ "Day 13 answer 2:\n" <> answer2 points folds

type Points = Set Point
type Point = (Int, Int)

type Folds = [Fold]
data Fold
    = AlongX Int
    | AlongY Int
    deriving (Eq, Ord, Show)

-- parse

parse :: String -> (Points, Folds)
parse str = case groups of
    [rawPoints, rawFolds] -> (parsePoints rawPoints, parseFolds rawFolds)
    _ -> error "invalid input"
    where
    groups = groupWith (isPrefixOf "fold along") $ filter ([] /=) $ lines str

parseFolds :: [String] -> Folds
parseFolds = map (toFold . splitOn "=" . flip (!!) 2 . splitOn " ")
    where
    toFold ["x", x] = AlongX (read x)
    toFold ["y", y] = AlongY (read y)
    toFold f = error $ "invalid fold: " <> show f

parsePoints :: [String] -> Points
parsePoints = Set.fromList . map toPoint
    where
    toPoint :: String -> Point
    toPoint p = case  splitOn "," p of
        [x, y] -> (read x, read y)
        _ -> error $ "invalid point: " <> p

-- fold

fold :: Points -> Fold -> Points
fold pts (AlongX x) = Set.map foldX $ Set.filter (\p -> x /= fst p) pts
    where 
    foldX :: Point -> Point
    foldX p@(x', y') | x' > x = (2 * x - x', y')
    foldX p = p
fold pts (AlongY y) = Set.map foldY $ Set.filter (\p -> y /= snd p) pts
    where
    foldY :: Point -> Point
    foldY p@(x', y') | y' > y = (x', 2 * y - y')
    foldY p = p

-- part 1 

answer1 :: Points -> Folds -> Int
answer1 pts flds = Set.size $ fold pts (head flds)
 
-- part 2

answer2 :: Points -> Folds -> String
answer2 pts flds = printSheet $ foldl' fold pts flds

printSheet :: Points -> String
printSheet pts = unlines $ chunksOf (maxX + 1) ascii
    where
    ascii = [ if Set.member (x, y) pts then '#' else ' ' | y <- [0 .. maxY], x <- [0 .. maxX]]
    ls = Set.toList pts 
    maxX = maximum $ map fst ls
    maxY = maximum $ map snd ls