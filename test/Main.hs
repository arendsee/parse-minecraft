{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import ParseMinecraft.ParseMCA (hexByte)
import ParseMinecraft.Parser (unsignedBigEndianInt)
import ParseMinecraft.Search (bitspread, bitspreadPadded)

import qualified Data.Word as W

main = do
  defaultMain $
    testGroup
      "all tests"
      [ allTests
      , numberTests
      , bitspreadTest
      ]

allTests =
  testGroup
    "hex tests"
    [ testCase "hexByte" $ assertEqual "" (hexByte 0) ("00")
    , testCase "hexByte" $ assertEqual "" (hexByte 1) ("01")
    , testCase "hexByte" $ assertEqual "" (hexByte 10) ("0a")
    , testCase "hexByte" $ assertEqual "" (hexByte 16) ("10")
    , testCase "hexByte" $ assertEqual "" (hexByte 255) ("ff")
    ]


numberTests =
  testGroup
    "number tests"
    [ testCase "unsigned int" $ assertEqual "" (unsignedBigEndianInt [0x00]) 0
    , testCase "unsigned int" $ assertEqual "" (unsignedBigEndianInt [0x01]) 1
    , testCase "unsigned int" $ assertEqual "" (unsignedBigEndianInt [0x02, 0x01]) 513
    , testCase "unsigned int" $ assertEqual "" (unsignedBigEndianInt [0x01, 0x02]) 258
    ]

bitspread' :: Int -> [W.Word64] -> [W.Word16]
bitspread' = bitspread 

bitspreadTest =
    testGroup
        "bitspread"
        [ testCase "1 []"   $ assertEqual "" (bitspread' 1 []) []
        , testCase "2 []"   $ assertEqual "" (bitspread' 64 []) []
        , testCase "64 [0]" $ assertEqual "" (bitspread' 64 [0]) [0]
        , testCase "64 [0,1,67,999]" $ assertEqual "" (bitspread' 64 [0,1,67,999]) [0,1,67,999]
        , testCase "8 [0]"  $ assertEqual "" (bitspread' 8 [0]) (take 8 (repeat 0))
        , testCase "16 [1]"  $ assertEqual "" (bitspread' 16 [1]) [0, 0, 0, 1]
        , testCase "16 [1,2]"  $ assertEqual "" (bitspread' 16 [1,2]) [0, 0, 0, 1, 0, 0, 0, 2]
        , testCase "20 [1]"  $ assertEqual "" (bitspread' 20 [1]) [0, 0, 0]
        , testCase "20 [1]"  $ assertEqual "" (bitspread' 20 [1]) [0, 0, 0]
        , testCase "" $ assertEqual "" (bitspread' 8 [2 ^ 63]) [128, 0, 0, 0, 0, 0, 0, 0]
        , testCase "1 [0]"  $ assertEqual "" (bitspread' 1 [0]) (take 64 (repeat 0))
        , testCase "32 [0]"  $ assertEqual "" (bitspread' 32 [0]) [0,0]
        , testCase "32 [1]"  $ assertEqual "" (bitspread' 32 [1]) [0,1]
        ]
