{-# LANGUAGE TupleSections #-}

module Day09 (day09) where

import Data.List (sort, sortBy, foldl', group)
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.Array (Array, listArray, (!))
import Data.Char (digitToInt)
import Control.Monad (guard)
import Data.Set (Set, fromList, member, (\\), size, singleton, insert, empty)
import qualified Data.Set as Set

day09 :: IO ()
day09 = do
    input <- readFile "data/day09.txt"
    let grid = parse input
    putStrLn $ "Day 9 answer 1: " <> show (answer1 grid)
    putStrLn $ "Day 9 answer 2: " <> show (answer2 grid)


-- parse

type Grid = Array Int Int

type X = Int
type Y = Int
type Z = Int
type Point = (X, Y, Z)

sizeX :: Int
sizeX = 100

sizeY :: Int
sizeY = 100

parse :: String -> Grid
parse = listArray (0, (sizeX * sizeY) - 1) . map digitToInt . mconcat . lines

getPointZ :: Grid -> Int -> Int -> Maybe Int
getPointZ grid x y | x >= 0 && x < sizeX && y >= 0 && y < sizeY = Just $ grid ! (x + y * sizeX)
getPointZ _ _ _ = Nothing

getNeighbours :: Grid -> X -> Y -> [Point]
getNeighbours grid x y = catMaybes [ (x - 1, y,) <$> getPointZ grid (x - 1) y
                                   , (x + 1, y,) <$> getPointZ grid (x + 1) y
                                   , (x, y - 1,) <$> getPointZ grid x (y - 1)
                                   , (x, y + 1,) <$> getPointZ grid x (y + 1)
                                   ]

findLows :: Grid -> [Point]
findLows grid = do
    x <- [0 .. sizeX - 1]
    y <- [0 .. sizeY - 1]
    let z = fromJust $ getPointZ grid x y
    let ns = map getZ $ getNeighbours grid x y
    guard $ all (z <) ns
    pure (x, y, z)

getZ :: Point -> Z
getZ (_, _, z) = z

--part 1 = 462

answer1 :: Grid -> Int
answer1 = sum . map ((+ 1) . getZ) . findLows

-- part 2 = 1397760

answer2 :: Grid -> Int
answer2 grid = product $ take 3 $ reverse $ sort $ map size basigns
    where
    basigns =  map (snd . toBasin (empty, empty)) $ findLows grid
    toBasin :: (Set Point, Set Point) -> Point -> (Set Point, Set Point)
    toBasin (visited, basign) p@(x, y, _) = (visited', insert p $ snd (foldl' toBasin (visited', basign) viableNeighbours))
        where
        viableNeighbours = filter (\n -> not (member n visited) && getZ n < 9) $ getNeighbours grid x y
        visited' = insert p (visited <> fromList viableNeighbours)

