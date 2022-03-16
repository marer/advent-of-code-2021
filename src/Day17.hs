module Day17 (day17) where

import Control.Monad (guard)

day17 :: IO ()
day17 = do
    putStrLn "Start..."
    putStrLn $ "Day 17 answer 1: " <> show (answer1 target)
    putStrLn $ "Day 17 answer 2: " <> show (answer2 target)

-- target: x=85..145, y=-163..-108

data Target = Target {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int}

type Point = (Int, Int)

type Trajectory = [Point]

type Velocity = (Int, Int)

target :: Target
target = Target{minX = 85, maxX = 145, minY = -163, maxY = -108}

answer1 :: Target -> Int
answer1 tgt = maximum maxYs
  where
    maxYs :: [Int]
    maxYs = do
        x <- [0 .. (maxX tgt)]
        y <- [0 .. (max (abs $ minY tgt) (abs $ maxY tgt))]
        let trajectory = simulate tgt (x, y)
        guard $ hitsTarget tgt trajectory
        pure $ maximum $ snd <$> trajectory

answer2 :: Target -> Int
answer2 tgt = length velocities
  where
    velocities :: [Velocity]
    velocities = do
        x <- [0 .. (maxX tgt)]
        y <- [minY tgt .. (max (abs $ minY tgt) (abs $ maxY tgt))]
        let trajectory = simulate tgt (x, y)
        guard $ hitsTarget tgt trajectory
        pure (x, y)

hitsTarget :: Target -> Trajectory -> Bool
hitsTarget tgt = any inTarget
  where
    inTarget :: Point -> Bool
    inTarget (x, y) =
        x >= minX tgt
            && x <= maxX tgt
            && y >= minY tgt
            && y <= maxY tgt

simulate :: Target -> Velocity -> Trajectory
simulate tgt = go (0, 0)
  where
    go :: Point -> Velocity -> Trajectory
    go (x, y) _ | x > maxX tgt || y < minY tgt = []
    go (x, y) (vx, vy) = nextPoint : go nextPoint nextV
      where
        nextPoint = (x + vx, y + vy)
        nextV = (max (vx - 1) 0, vy - 1)