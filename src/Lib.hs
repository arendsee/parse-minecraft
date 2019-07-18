module Lib ( parseMCA ) where

import Data.Attoparsec.ByteString
import Data.Word (Word8)
import qualified Data.ByteString as B
import Codec.Compression.Zlib (compress, decompress)
import Data.List (intersperse)
import Data.ByteString.Base16 (encode, decode)

type Location = (Word8, Word8, Word8, Word8)
type Timestamp = (Word8, Word8, Word8, Word8)

parseMCA :: B.ByteString -> B.ByteString
parseMCA x = case parseOnly toplevel x of
  (Left msg) -> error msg
  (Right xs) -> packout xs <> "\n"

packout :: [(Location, Timestamp)] -> B.ByteString
packout = B.intercalate "\n" . map encode . map write' where
  write' :: (Location, Timestamp) -> B.ByteString
  write' ((a,b,c,d),_) = B.pack [a,b,c,d]

toplevel :: Parser [(Location, Timestamp)]
toplevel = do
  locations <- count 1024 location
  timestamps <- count 1024 timestamp
  skipWhile true
  return (zip locations timestamps)

location :: Parser Location
location = do
  x0 <- anyWord8
  x1 <- anyWord8
  x2 <- anyWord8
  x3 <- anyWord8
  return (x0,x1,x2,x3)

timestamp :: Parser Timestamp
timestamp = do
  x0 <- anyWord8
  x1 <- anyWord8
  x2 <- anyWord8
  x3 <- anyWord8
  return (x0,x1,x2,x3)

true :: a -> Bool
true _ = True

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
