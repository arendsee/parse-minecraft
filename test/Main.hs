{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import ParseMinecraft.Lib (hexByte)

main = do
  defaultMain $
    testGroup
      "all tests"
      [ allTests
      ]

allTests =
  testGroup
    "Test of test"
    [ testCase "hexByte" $ assertEqual "" (hexByte 0) ("00")
    , testCase "hexByte" $ assertEqual "" (hexByte 1) ("01")
    , testCase "hexByte" $ assertEqual "" (hexByte 10) ("0a")
    , testCase "hexByte" $ assertEqual "" (hexByte 16) ("10")
    , testCase "hexByte" $ assertEqual "" (hexByte 255) ("ff")
    ]
