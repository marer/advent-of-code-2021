module Day15 (day15) where

import Control.Monad.State.Strict (State, get, gets, modify', foldM_, evalState, forM)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Char (digitToInt, intToDigit)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ

day15 :: IO ()
day15 = do
    putStrLn "Start..."
    input <- readFile "data/day15.txt"
    putStrLn $ "Day 15 answer 1: " <> show (answer1 input)
    putStrLn $ "Day 15 answer 2: " <> show (answer2 input)

answer1 :: String -> Maybe Int
answer1 input = dijkstra graph (0,0) (99,99)
    where
    graph = toGraphData $ parseGrid input

answer2 :: String -> Maybe Int
answer2 input = dijkstra graph (0,0) (499,499)
    where
    graph = toGraphData $ parseGrid $ unlines $ extendGrid 5 $ lines input

    extendGrid :: Int -> [String] -> [String]
    extendGrid times = extendBlock . List.map extendLine
        where
        extendLine :: String -> String
        extendLine = extend (List.map . increaseBy)

        extendBlock :: [String] -> [String]
        extendBlock = extend (List.map . List.map . increaseBy)

        extend fn = mconcat . zipWith fn [0..] . replicate times

        increaseBy :: Int -> Char -> Char
        increaseBy i = intToDigit . doIncrease . digitToInt
            where
            doIncrease j | i + j < 10 = i + j
            doIncrease j = 1 + mod (i + j) 10


type MapGrid = Map Int (Map Int Int)

type Point = (Int, Int)
type Size = Int
type GraphData = (Vertices, Edges)
type Vertices = [Point]
type Edges = Map (Point, Point) Int

type Remaining = Set Point
type Costs = PSQ Point Int

--parse

parseGrid :: String -> MapGrid
parseGrid = Map.fromList . zip [0..] . map parseLine . lines
    where
    parseLine :: String -> Map Int Int
    parseLine = Map.fromList . zip [0..] . map digitToInt

toGraphData :: MapGrid -> GraphData
toGraphData grid = (vertices, edges)
    where
    size = Map.size grid
    vertices =  [(x, y) | x <- [0..size-1], y <- [0..size-1]]
    edges = Map.fromList $ do
        point <- vertices
        neighbour <- getNeighbours size point
        let weight = getPoint grid point
        pure ((neighbour, point), weight)

getPoint :: MapGrid -> Point -> Int
getPoint grid (x, y) = (grid ! x) ! y

getNeighbours :: Int -> Point -> [Point]
getNeighbours size (x, y) = List.filter isValid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    where
    isValid (x, y) = x >= 0 && x < size && y >= 0 && y < size

-- dijkstra

type Process a = State (Costs, Remaining) a

dijkstra :: GraphData -> Point -> Point -> Maybe Int
dijkstra (vertices, edges) start exit = evalState (process start) (initialCosts, initialRemaining)
    where
    size = 1 + fst (List.last vertices)

    process :: Point -> Process (Maybe Int)
    process point | point == exit = gets ( PQ.lookup point . fst)
    process point = do
        currentCost <- gets (fromJust . PQ.lookup point . fst)
        neighbours <- gets (getUnvisitedNeighbours point . snd)
        applyNeighbourCosts currentCost point neighbours
        markAsVisited point
        next <- getNext
        case next of
            Just p -> process p
            Nothing -> pure Nothing

    getUnvisitedNeighbours :: Point -> Remaining -> [Point]
    getUnvisitedNeighbours point remaining = filter (`Set.member` remaining) $ getNeighbours size point

    applyNeighbourCosts :: Int -> Point -> [Point] -> Process ()
    applyNeighbourCosts currentCost point neighbours = foldM_ modifyCost () neighbourCosts
        where
        neighbourCosts = toNeighbourCost <$> neighbours

        toNeighbourCost :: Point -> (Point, Int)
        toNeighbourCost neighbour = (neighbour, currentCost + edges ! (point, neighbour))

        modifyCost :: () -> (Point, Int) -> Process ()
        modifyCost _ (pt, cost) = modify' (first (PQ.alter doModify pt))
            where
            doModify Nothing = Just cost
            doModify (Just existing) = Just (min cost existing)

    markAsVisited :: Point -> Process ()
    markAsVisited point = modify' (bimap (PQ.delete point) (Set.delete point))

    getNext :: Process (Maybe Point)
    getNext = do
        costs <- gets fst
        pure $ PQ.key <$> PQ.findMin costs

    initialCosts :: Costs
    initialCosts = PQ.singleton start 0

    initialRemaining :: Remaining
    initialRemaining = Set.fromList vertices
