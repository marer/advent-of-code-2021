{-# LANGUAGE OverloadedStrings #-}
module Day16 (day16, answer1, answer2) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Writer (Writer, MonadWriter (tell), execWriter, foldM_)
import Data.Bits (testBit)
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first)
import Data.Word (Word8)
import Text.Hex (encodeHex, decodeHex)
import Debug.Trace (trace)

day16 :: IO ()
day16 = do
    putStrLn "Start..."
    input <- TIO.readFile "data/day16.txt"
    putStrLn $ "Day 16 answer 1: " <> show (answer1 input)
    putStrLn $ "Day 16 answer 2: " <> show (answer2 input)

-- answer 1

answer1 :: T.Text -> Int
answer1 = sum . execWriter . sumVersions . fst . toPacket . parse
    where
    sumVersions :: Packet -> Writer [Int] ()
    sumVersions (Packet version (Literal _)) = tell [version]
    sumVersions (Packet version (Operator _ pkts)) = do
        tell [version]
        foldM_ (const sumVersions) () pkts

-- answer 2

answer2 :: T.Text -> Int
answer2 = eval . fst . toPacket . parse
    where
    eval :: Packet -> Int
    eval (Packet _ (Literal val)) = val
    eval (Packet _ (Operator 0 ps)) = sum $ eval <$> ps
    eval (Packet _ (Operator 1 ps)) = product $ eval <$> ps
    eval (Packet _ (Operator 2 ps)) = minimum $ eval <$> ps
    eval (Packet _ (Operator 3 ps)) = maximum $ eval <$> ps
    eval (Packet _ (Operator 5 [p1, p2])) = if eval p1 > eval p2 then 1 else 0
    eval (Packet _ (Operator 6 [p1, p2])) = if eval p1 < eval p2 then 1 else 0
    eval (Packet _ (Operator 7 [p1, p2])) = if eval p1 == eval p2 then 1 else 0
    eval (Packet _ (Operator op pts)) = error $ "invalid packet: " <> show op <> " " <> show (length pts)
    

-- parse

type RawData = [Bool]

showAsBinary :: RawData -> String
showAsBinary = foldl (\acc b -> acc <> (if b then  "1" else "0")) ""

parse :: T.Text -> RawData
parse = concatMap toRawData . B.unpack . fromJust . decodeHex
    where
    toRawData :: Word8 -> RawData
    toRawData w = map (testBit w) [7, 6..0]

toInt :: RawData -> Int
toInt = sum . zipWith (\i b -> 2^i * if b then 1 else 0) [0..] . reverse

-- Packet

data Packet = 
    Packet
    { version :: Int
    , value :: PacketValue
    }
    deriving (Show)


data PacketValue 
    = Literal Value
    | Operator TypeId [Packet]
    deriving (Show)

type Version = Int
type Value = Int
type TypeId = Int

toPacket :: RawData -> (Packet, RawData)
toPacket raw = (packet, remaining)
    where
    packet = Packet { version = version, value = value } 
    version = toInt $ take 3 raw
    typeId = toInt $ take 3 $ drop 3 raw
    contents = drop 6 raw
    (value, remaining) = 
        if typeId == 4 
            then first Literal $ parseLiteralValue contents
            else first (Operator typeId) $ parseOperatorContents contents


parseLiteralValue :: RawData -> (Int, RawData)
parseLiteralValue raw = first toInt $ doSplit (chunksOf 5 raw) ([], [])
    where
    doSplit [] result = result
    doSplit (firstChunk : rest) (rawLit, rs) = 
        if head firstChunk 
            then doSplit rest (rawLit <> tail firstChunk, [])
            else (rawLit <> tail firstChunk, concat rest)

parseOperatorContents :: RawData -> ([Packet], RawData)
parseOperatorContents (False : contents) = (doParse packetsRaw [], remaining)
    where
    packetsLen = toInt $ take 15 contents
    packetsRaw = take packetsLen $ drop 15 contents
    remaining = drop (15 + packetsLen) contents
    
    doParse :: RawData -> [Packet] -> [Packet]
    doParse [] acc = acc
    doParse raw acc = doParse rem (acc <> [p])
        where
        (p, rem) = toPacket raw
parseOperatorContents (True : contents) = doParse pktsCount [] pktsRaw
    where
    pktsCount = toInt $ take 11 contents
    pktsRaw = drop 11 contents
    
    doParse :: Int -> [Packet] -> RawData -> ([Packet], RawData)
    doParse cnt acc raw | cnt < 1 = (acc, raw)
    doParse cnt acc raw = doParse (cnt - 1) (acc <> [pkt]) rem
        where 
        (pkt, rem) = toPacket raw
parseOperatorContents _ = error "invalid contents"
