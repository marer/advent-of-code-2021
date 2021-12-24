
{-# LANGUAGE LambdaCase #-}

module Day11 (day11) where

import Data.Array (Array, listArray, (!), (//))
import Data.Char (digitToInt)
import Control.Monad.State (StateT, evalStateT, modify', gets)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Control.Monad (mapM_, guard, when)
import Debug.Trace (trace)

day11 :: IO ()
day11 = do
    input <- readFile "data/day11.txt"
    let grid = parse input
    putStrLn $ "Day 11 answer 1: " <> show (answer1 grid)
    putStrLn $ "Day 11 answer 2: " <> show (answer2 grid)
    
-- parse 

sizeX :: Int
sizeX = 10
sizeY :: Int
sizeY = 10

type OctoGrid = Array Int Octo
data Octo = Level Int | Flashing
type Pos = (Int, Int)
type App = StateT OctoGrid (Writer [Int]) 

parse :: String -> OctoGrid
parse = listArray (0, (sizeX * sizeY) - 1) . map (Level . digitToInt) . mconcat . lines

getAt ::  Pos -> OctoGrid -> Octo
getAt (x, y) grid = grid ! (x + y * sizeX)

setAt :: Pos -> Octo -> OctoGrid -> OctoGrid
setAt (x, y) val grid = grid // [(x + y * sizeX, val)] 

-- common

increaseLevel :: Pos -> App ()
increaseLevel pos = do
  octo <- gets $ getAt pos
  case octo of
    (Level i) | i < 9 -> modify' $ setAt pos (Level (i + 1))
    (Level _) -> do
      modify' $ setAt pos Flashing
      mapM_ increaseLevel (getNeighbours pos)
    Flashing -> pure ()  

getNeighbours :: Pos -> [Pos]
getNeighbours p@(x, y) = do
  x' <- [x - 1 .. x + 1]
  y' <- [y - 1 .. y + 1]
  guard $ (x', y') /= p && 0 <= x' && x' < sizeX && 0 <= y' && y' < sizeY
  pure (x', y')

countFlashing :: OctoGrid -> Int
countFlashing = sum . fmap
    (\case 
        Flashing -> 1
        _ -> 0
    )

upkeep :: OctoGrid -> OctoGrid
upkeep = fmap 
    (\case
       Flashing -> Level 0
       o -> o
    )


allPos :: [Pos]
allPos = [(x, y) | x <- [0 .. sizeX - 1], y <- [0 .. sizeY - 1]]

-- part 1
 
answer1 :: OctoGrid -> Int
answer1 octoGrid = sum $ execWriter $ evalStateT app octoGrid
    where
    app :: App ()
    app = mapM_ processStep [1 .. 100]
    processStep :: Int -> App ()
    processStep _ = do
        mapM_ increaseLevel allPos
        flashing <- gets countFlashing
        tell [flashing]
        modify' upkeep

-- part 2

answer2 :: OctoGrid -> Int
answer2 octoGrid = fst $ runWriter $ evalStateT (processStep 1) octoGrid
    where
    processStep :: Int -> App Int
    processStep i = do
        mapM_ increaseLevel allPos
        flashing <- gets countFlashing
        modify' upkeep
        if flashing == sizeX * sizeY 
          then pure i
          else processStep (i + 1)