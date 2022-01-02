module Main where

import ParseMinecraft
import qualified Data.ByteString as B
import System.Environment
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide a single *.mca filename"
    (filename : _) -> do
      mca <- fmap parseMCA (B.readFile filename)
      case mca of
        Right (locations, timestamps, chunks, tags) ->
          -- putStrLn $ show $ length $ locations
          -- putStrLn $ show $ length $ timestamps
          -- putStrLn $ show $ length $ chunks
          pPrint $ tags
        Left msg -> putStr msg
