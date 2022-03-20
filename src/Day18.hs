module Day18 (day18) where

import Control.Monad.State (MonadState (get, put), State, execState, foldM, modify)
import Data.Char (isDigit)
import GHC.Base (Alternative ((<|>)), Semigroup)

day18 :: IO ()
day18 = do
    putStrLn "Start..."
    numbers <- parse <$> readFile "data/day18.txt"
    putStrLn $ "Day 18 answer 1: " <> show (answer1 numbers)
    putStrLn $ "Day 18 answer 2: " <> show (answer2 numbers)

-- answers

answer1 :: [Number] -> Int
answer1 = magnitudeOf . sumNumbers
  where
    sumNumbers = foldl1 (<>)

answer2 :: [Number] -> Int
answer2 = maximum . map (magnitudeOf . sumPair) . toPairs
  where
    toPairs :: [a] -> [(a, a)]
    toPairs (f : rest) = [(f, s) | s <- rest] <> [(s, f) | s <- rest] <> toPairs rest
    toPairs _ = []
    sumPair (f, s) = f <> s

magnitudeOf :: Number -> Int
magnitudeOf (Literal val) = val
magnitudeOf (Pair l r) = 3 * magnitudeOf l + 2 * magnitudeOf r

-- model

data Number
    = Literal Int
    | Pair Number Number

instance Show Number where
    show (Literal v) = show v
    show (Pair l r) = "[" <> show l <> "," <> show r <> "]"

type Parser a = State [Number] a

instance Semigroup Number where
    (<>) (Literal l) (Literal r) = reduce $ Literal (l + r)
    (<>) l r = reduce $ Pair l r

reduce :: Number -> Number
reduce num = maybe num reduce (explode num <|> split num)

explode :: Number -> Maybe Number
explode num = fst <$> go 0 num
  where
    fst (num, _, _) = num
    go :: Int -> Number -> Maybe (Number, Int, Int)
    go _ (Literal _) = Nothing
    go depth (Pair (Literal l) (Literal r)) | depth >= 4 = Just (Literal 0, l, r)
    go depth (Pair l r) = (explodeLeft <$> go (depth + 1) l) <|> (explodeRight <$> go (depth + 1) r)
      where
        explodeLeft (newL, remL, remR) = (Pair newL (addLeft remR r), remL, 0)
        explodeRight (rewR, remL, remR) = (Pair (addRight remL l) rewR, 0, remR)

    addLeft :: Int -> Number -> Number
    addLeft i (Literal val) = Literal (val + i)
    addLeft i (Pair l r) = Pair (addLeft i l) r

    addRight :: Int -> Number -> Number
    addRight i (Literal val) = Literal (val + i)
    addRight i (Pair l r) = Pair l (addRight i r)

split :: Number -> Maybe Number
split (Literal v) | v >= 10 = Just $ Pair (Literal (v `div` 2)) (Literal (succ v `div` 2))
split (Literal _) = Nothing
split (Pair l r) = (`Pair` r) <$> split l <|> Pair l <$> split r

-- parse

parse :: String -> [Number]
parse = map parseNumber . lines

parseNumber :: String -> Number
parseNumber str = head $ execState (foldM go () str) []
  where
    go :: () -> Char -> Parser ()
    go _ '[' = pure ()
    go _ ',' = pure ()
    go _ ']' = do
        right <- popStack
        left <- popStack
        let val = Pair left right
        pushStack val
    go _ d | isDigit d = do
        let val = Literal (read [d])
        pushStack val
    go _ c = error $ "invalid character" <> show c
    popStack :: Parser Number
    popStack = do
        stack <- get
        case stack of
            (val : rest) -> do
                put rest
                pure val
            _ -> error "invalid expression"
    pushStack :: Number -> Parser ()
    pushStack val = modify (val :)
