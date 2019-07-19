module Lib ( parseMCA ) where

import Prelude hiding (take)
import Data.Attoparsec.ByteString
import Data.Word (Word8)
import Data.Bits
import qualified Data.ByteString as B
import qualified Codec.Compression.Zlib as Zlip
import Data.List (intersperse)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy.Char8 as BL

type Location = (Int, Int)
type Timestamp = (Word8, Word8, Word8, Word8)
type Chunk = B.ByteString

parseMCA :: B.ByteString -> B.ByteString
parseMCA x = case parseOnly toplevel x of
  (Left msg) -> error msg
  (Right xs) -> B.concat xs

toplevel :: Parser [Chunk]
toplevel = do
  _ <- count 1024 location
  _ <- count 1024 timestamp
  chunks <- many1 chunk
  return chunks

location :: Parser Location
location = do
  loc <- int 3
  sectors <- int 1
  return (loc, sectors)

timestamp :: Parser Int
timestamp = int 4

chunk :: Parser Chunk
chunk = do
  chunkLength <- int 4
  compressionType <- satisfy (\w -> w == 1 || w == 2)
  compressedChunk <- take chunkLength
  padding <- take (4096 - (mod chunkLength 4096))
  return $ decompress compressedChunk

decompress :: B.ByteString -> B.ByteString
decompress = BL.toStrict . Zlip.decompress . BL.fromStrict

int :: Int -> Parser Int
int i = do
  xs <- count i anyWord8
  return . sum $ zipWith sint [0,8..] (reverse xs)
  where
    sint :: Int -> Word8 -> Int
    sint i w = shiftL (fromIntegral w) i


{-----------------------------------------------

# MCA file format

See here: https://minecraft.gamepedia.com/Region_file_format

Region file format:

--------------|-------|-------|
              | start |  stop |
--------------|-------|-------|
locations     |    0  |  4095 |
timestamps    | 4096  |  8191 |
chunks/unused | 8192  |  x    |
--------------|-------|-------|

mca = locations + timestamps + chunks
  locations = take 1024 location
    location = offset + sectorCount
      offset = take 3 byte       -- multiple of 4kiB offset for a chunk start position
      sectorCount = take 1 byte  -- number of sectors in the chunk
 
  timestamps = take 1024 timestamp
    timestamp = take 4 byte

  chunks = take n chunk
    chunk = length + compressionType + data
      length = take 4 byte
      compressionType = take 1 byte
      data = take length byte                -- compressed data

Each chunk will need to be individually decompressed and then parsed.

# Chunk format

https://minecraft.gamepedia.com/Chunk_format

-----------------------------------------------}
