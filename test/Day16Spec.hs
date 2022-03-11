{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Test.Hspec
import Day16

spec :: Spec
spec = do
    describe "answer 1" $ do
        it "should parse literal (D2FE28)" $ do
            answer1 "D2FE28" `shouldBe` 6

        it "should parse nested literal (8A004A801A8002F478)" $ do
            answer1 "8A004A801A8002F478" `shouldBe` 16

        it "should parse operator with multiple subpackets (620080001611562C8802118E34)" $ do
            answer1 "620080001611562C8802118E34" `shouldBe` 12
        
        it "should parse operator with multiple subpackets ver 2 (C0015000016115A2E0802F182340)" $ do
            answer1 "C0015000016115A2E0802F182340" `shouldBe` 23
        
        it "should nested multiple literals (A0016C880162017C3686B18A3D4780)" $ do
            answer1 "A0016C880162017C3686B18A3D4780" `shouldBe` 31
    describe "answer 2" $ do
        it "should calculate sum" $ do
            answer2 "C200B40A82" `shouldBe` 3

        it "should calculate product" $ do
            answer2 "04005AC33890" `shouldBe` 54

        it "should calculate minimum" $ do
            answer2 "880086C3E88112" `shouldBe` 7

        it "should calculate maximum" $ do
            answer2 "CE00C43D881120" `shouldBe` 9

        it "should calculate lt" $ do
            answer2 "D8005AC2A8F0" `shouldBe` 1

        it "should calculate gt" $ do
            answer2 "F600BC2D8F" `shouldBe` 0

        it "should calculate eq" $ do
            answer2 "9C005AC2F8F0" `shouldBe` 0

        it "should calculate 1 + 3 = 2 * 2" $ do
            answer2 "9C0141080250320F1802104A08" `shouldBe` 1