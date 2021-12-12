module Day2 (day2) where

import Prelude
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Control.Monad.State (State, execState, modify)

day2 :: IO ()
day2 = do
    input <- readFile "data/day2.txt"
    let commands = catMaybes $ parseCommand <$> lines input
    putStrLn $ "Day 2 answer 1: " <> show (process1 commands)
    putStrLn $ "Day 2 answer 2: " <> show (process2 commands)


-- parsing

type Command = (Action, Int)

data Action = Up | Down | Forward

parseCommand :: String -> Maybe Command
parseCommand str =
    case splitOn " " str of
        [ actionStr, value ] -> do
            action <- parseAction actionStr
            pure (action, read value)
        _ -> Nothing

parseAction :: String -> Maybe Action
parseAction "up" = Just Up
parseAction "down" = Just Down
parseAction "forward" = Just Forward
parseAction _ = Nothing

-- part 1

process1 :: [Command] -> Int
process1 cmds = coordinatesOf $ execState (mapM_ followCommand cmds) (0, 0)

followCommand :: Command -> State Position ()
followCommand (Up, val) = modify $ goUp val
followCommand (Down, val) = modify $ goDown val
followCommand (Forward, val) = modify $ goForward val
        
coordinatesOf :: Position -> Int
coordinatesOf (d, h) =  d * h

type Depth = Int
type Horizontal = Int

type Position = (Depth, Horizontal)

goUp :: Int -> Position -> Position
goUp val (d, h) = (max 0 (d - val), h)

goDown :: Int -> Position -> Position
goDown val (d, h) = (max 0 (d + val), h)

goForward :: Int -> Position -> Position
goForward val (d, h) = (d, h + val)

--- part 2

type Aim = Int
type SubState = (Position, Aim)


process2 :: [Command] -> Int
process2 cmds =  coordinatesOf $ fst $ execState (mapM_ followCommand' cmds) ((0, 0), 0)

followCommand' :: Command -> State SubState ()
followCommand' (Up, val) = modify $ aimUp val
followCommand' (Down, val) = modify $ aimDown val
followCommand' (Forward, val) = modify $ goForward' val

aimUp :: Int -> SubState -> SubState
aimUp val (pos, aim) = (pos, aim - val)

aimDown :: Int -> SubState -> SubState
aimDown val (pos, aim) = (pos, aim + val)

goForward' :: Int -> SubState -> SubState
goForward' val (pos, aim) = (followAim pos, aim)
    where 
    followAim = goDown (val * aim) . goForward val