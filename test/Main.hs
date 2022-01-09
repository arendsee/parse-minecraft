{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import ParseMinecraft.Lib (hexByte)
import ParseMinecraft.Numbers (unsignedBigEndianInt)

import qualified Data.Word as W

main = do
  defaultMain $
    testGroup
      "all tests"
      [ allTests
      , numberTests
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
