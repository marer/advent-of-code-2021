{-# LANGUAGE BangPatterns #-}
module Day05 (day05) where

import Data.Array ((!), (//), array, elems, Array)
import Data.List.Split (splitOn)
import Data.List.Split.Internals (splitOn)
import Data.Array.Base (listArray)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map

day05 :: IO ()
day05 = do
    input <- readFile "data/day05.txt"
    let lns = parseLines input
    putStrLn $ "Day 5 answer 1: " <> show (answer1 emptyMapGrid lns)
    putStrLn $ "Day 5 answer 2: " <> show (answer2 emptyMapGrid lns)

-- common

gridSize = 1000 :: Int

type Point = (Int, Int)
type Line = (Point, Point)

parseLines :: String -> [Line]
parseLines = map parseLine . lines
    where
    parseLine l = case splitOn " -> " l of
        [p1, p2] -> (parsePoint p1, parsePoint p2)
        _ -> error $ "invalid input: " <> l
    parsePoint p = case splitOn "," p of
        [x, y] -> (read x, read y)
        _ -> error $ "invalid input: " <> p

class GridAccess a where
    markPoint :: a -> Point -> a
    countOverlaps :: a -> Int

-- ArrayGrid (meh)

newtype ArrayGrid = ArrayGrid { getArray :: Array Int Word }

emptyArrayGrid :: ArrayGrid
emptyArrayGrid = ArrayGrid $ listArray (0, gridSize^2-1) (repeat 0)

instance GridAccess ArrayGrid where
    markPoint (ArrayGrid g) (x, y) = ArrayGrid $ g // [(index, current + 1)]
        where
        index = x + y * gridSize
        current = g ! index
    countOverlaps = foldl' go 0 . getArray
        where
        go !acc i | i > 1 = acc + 1
        go !acc _ = acc

-- MapGrid

newtype MapGrid = MapGrid { getMap :: Map.Map Point Int }
    deriving (Show)

emptyMapGrid :: MapGrid
emptyMapGrid = MapGrid Map.empty 

instance GridAccess MapGrid where
    markPoint grid point = MapGrid $ Map.insertWith (+) point 1 $ getMap grid
    countOverlaps = length . filter (> 1) . Map.elems . getMap

-- part 1

answer1 :: GridAccess a => a -> [Line] -> Int
answer1 grid = countOverlaps . foldl markLine grid . filter isLine 
    where
    isLine (p1, p2) = fst p1 == fst p2 || snd p1 == snd p2
    markLine grid ((x1, y1), (x2, y2)) = foldl' markPoint grid points
        where 
        points = do
            x <- [(min x1 x2) .. (max x1 x2)]
            y <- [(min y1 y2) .. (max y1 y2)]
            pure (x, y)

-- part 2

answer2 :: GridAccess a => a -> [Line] -> Int
answer2 grid = countOverlaps . foldl markLine grid
    where
    markLine grid ((x1, y1), (x2, y2)) = foldl' markPoint grid points
        where 
        points = go (x1, y1) []
        go p pts | p == (x2, y2) = p : pts
        go p@(cx, cy) pts = go (moveUnless cx dx x2, moveUnless cy dy y2) (p : pts)
        dx = if x1 < x2 then 1 else -1
        dy = if y1 < y2 then 1 else -1
        moveUnless a delta end = if a /= end then a + delta else a
