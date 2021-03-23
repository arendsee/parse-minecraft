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
import Control.Applicative ((<|>))

type Location = (Int, Int)
type Timestamp = (Word8, Word8, Word8, Word8)
type Chunk = B.ByteString

parseMCA :: B.ByteString -> B.ByteString
parseMCA x = case parseOnly toplevel x of
  (Left msg) -> error msg
  (Right xs) -> B.concat xs

parseNBT :: B.ByteString -> B.ByteString
parseNBT x = case parseOnly nbt x of
  (Left msg) -> error msg
  (Right xs) -> xs

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

data Tag
  = TagEnd 
  | TagByte Word8
  | TagShort Int
  | TagInt Int
  | TagLong Int
  | TagFloat Double
  | TagDouble Double
  | TagByteArray [Word8]
  | TagString B.ByteString
  | TagList [Tag]
  | TagCompound [(Name, Tag)]

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 0  NAME: TAG_End
-- Payload: None.
-- Note: This tag is used to mark the end of a list.  Cannot be named!
--       If type 0 appears where a Named Tag is expected, the name is assumed
--       to be "".  (In other words, this Tag is always just a single 0 byte
--       when named, and nothing in all other cases)
tag_end :: Parser Tag
tag_end = do
  _ <- word8 0x00
  return TagEnd

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 1  NAME: TAG_Byte
-- Payload: A single signed byte (8 bits)
tag_byte :: Parser Tag
tag_byte = do
  _ <- word8 0x01
  x <- word8
  return $ TagByte x

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 2  NAME: TAG_Short
-- Payload: A signed short (16 bits, big endian)
tag_short :: Parser Tag
tag_short = do
  _ <- word8 0x02
  nameLength <- int 2
  name <- take nameLength
  i <- int 2
  return $ TagShort i

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 3  NAME: TAG_Int
-- Payload: A signed short (32 bits, big endian)
tag_int :: Parser Tag
tag_int = do
  _ <- word8 0x03
  nameLength <- int 2
  name <- take nameLength
  i <- int 4
  return $ TagShort i 

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 4  NAME: TAG_Long
-- Payload: A signed long (64 bits, big endian)
tag_long :: Parser Tag
tag_long = do
  _ <- word8 0x04
  nameLength <- int 2
  name <- take nameLength
  i <- int 8
  return $ TagLong i

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 5  NAME: TAG_Float
-- Payload: A floating point value (32 bits, big endian, IEEE 754-2008, binary32)
tag_float :: Parser Tag
tag_float = word8 0x05

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 6  NAME: TAG_Double
-- Payload: A floating point value (64 bits, big endian, IEEE 754-2008, binary64)
tag_double :: Parser Tag
tag_double = word8 0x06

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 7  NAME: TAG_Byte_Array
-- Payload: TAG_Int length
--          An array of bytes of unspecified format. The length of this array is <length> bytes
tag_byte_array :: Parser Tag
tag_byte_array = word8 0x07

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 8  NAME: TAG_String
-- Payload: TAG_Short length
--          An array of bytes defining a string in UTF-8 format. The length of this array is <length> bytes
tag_string :: Parser Tag
tag_string = word8 0x08

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 9  NAME: TAG_List
-- Payload: TAG_Byte tagId
--          TAG_Int length
--          A sequential list of Tags (not Named Tags), of type <typeId>.
--          The length of this array is <length> Tags
-- Notes:   All tags share the same type.
tag_list :: Parser Tag
tag_list = word8 0x09

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 10 NAME: TAG_Compound
-- Payload: A sequential list of Named Tags. This array keeps going until a TAG_End is found.
--          TAG_End end
-- Notes:   If there's a nested TAG_Compound within this tag, that one will also
--          have a TAG_End, so simply reading until the next TAG_End will not work.
--          The names of the named tags have to be unique within each TAG_Compound
--          The order of the tags is not guaranteed.
tag_compound :: Parser Tag
tag_compound = word8 0x0a


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
