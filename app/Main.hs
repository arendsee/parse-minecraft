module Main where

import ParseMinecraft
import qualified Data.ByteString as B
import System.Environment
import Text.Pretty.Simple (pPrint)

printPair :: (Show a, Show b) => [(a, b)] -> IO ()
printPair [] = return ()
printPair ((x, y):rs) = do
  putStrLn $ show x ++ "\t" ++ show y
  printPair rs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide a single *.mca filename"
    (filename : _) -> do
      mca <- fmap parseMCA (B.readFile filename)
      case mca of
        Right (locations, timestamps, chunks, tags) -> pPrint tags
        -- Right (locations, timestamps, chunks, tags) -> printPair $ map chunkXZ tags
        Left msg -> putStr msg
