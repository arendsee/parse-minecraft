module Main where

import Lib
import Data.ByteString as B

main :: IO ()
main = B.interact parseMCA
