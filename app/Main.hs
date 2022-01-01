module Main where

import Lib
import qualified Data.ByteString as B
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide a single *.mca filename"
    (filename : _) -> do
      mca <- fmap parseMCA (B.readFile filename)
      case mca of
        Right (locations, timestamps, chunks) ->
          -- putStrLn $ show $ length $ locations
          -- putStrLn $ show $ length $ timestamps
          -- putStrLn $ show $ length $ chunks
          putStrLn . show $ chunks 
        Left msg -> putStr msg
