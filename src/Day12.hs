module Day12 (day12) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)

day12 :: IO ()
day12 = do
    input <- readFile "data/day12.txt"
    let connections = parse input
    putStrLn $ "Day 12 answer 1: " <> show (answer1 connections)
    putStrLn $ "Day 12 answer 2: " <> show (answer2 connections)


data Cave
    = Start
    | End
    | Small String
    | Big String
    deriving (Eq, Ord, Show)

type Connections = Map Cave (Set Cave)

type Path = [Cave]

parse :: String -> Connections
parse = foldl toConnections Map.empty . map parseLine . lines
    where
    parseLine :: String -> (Cave, Cave)
    parseLine line = case splitOn "-" line of
        [f, s] -> (parseCave f, parseCave s)
        _ -> error $ "invalid line: " <> line

    parseCave :: String -> Cave
    parseCave "start" = Start
    parseCave "end" = End
    parseCave name | all isUpper name = Big name
    parseCave name  = Small name

    toConnections :: Connections -> (Cave, Cave) -> Connections
    toConnections acc (End, s) = Map.insertWith (<>) s (Set.singleton End) acc
    toConnections acc (f, End) = Map.insertWith (<>) f (Set.singleton End) acc
    toConnections acc (Start, s) = Map.insertWith (<>) Start (Set.singleton s) acc
    toConnections acc (f, Start) = Map.insertWith (<>) Start (Set.singleton f) acc
    toConnections acc (f, s) = Map.insertWith (<>) s (Set.singleton f) $ Map.insertWith (<>) f (Set.singleton s) acc

uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

removeCave :: Connections -> Cave -> Connections
removeCave conns c@(Small _) = Set.delete c <$> Map.delete c conns
removeCave conns _ = conns

-- part 1

answer1 :: Connections -> Int
answer1 = length . findPaths Start
    where
    findPaths :: Cave -> Connections -> [[Cave]]
    findPaths End _ = [[End]]
    findPaths cave conns = do
        next <-  maybe [] Set.toList $ Map.lookup cave conns
        rest <- findPaths next (removeCave conns cave)
        pure (cave : rest)

-- part 2

answer2 :: Connections -> Int
answer2 = length . uniq . map (snd <$>) .findPaths (False, Start)
    where
    findPaths :: (Bool, Cave) -> Connections -> [[(Bool, Cave)]]
    findPaths c@(_, End) _ = [[c]]
    findPaths c@(twiceUsed, cave) conns = do
        next <- maybe [] Set.toList $ Map.lookup cave conns
        (newTwiceUsed, newConns) <- [(twiceUsed, removeCave conns cave)] <> [(True, conns) | not twiceUsed]
        rest <- findPaths (newTwiceUsed, next) newConns
        pure (c : rest)
