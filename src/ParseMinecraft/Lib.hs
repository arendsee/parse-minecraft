module ParseMinecraft.Lib ( parseMCA, hexByte) where

import Prelude hiding (take)
import Data.Attoparsec.ByteString
import Data.Word (Word8, Word32, Word64)
import Data.Bits
import qualified Data.ByteString as B
import qualified Codec.Compression.Zlib as Zlib
import Data.List (intersperse)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified Data.ByteString as DB
import Control.Applicative ((<|>))

import ParseMinecraft.Numbers (signed_int, unsigned_int, float, double)
import ParseMinecraft.Namespace

 -- MCA format is for descirbing 32x32 chunk regions
parseMCA :: B.ByteString -> Either String ([Location], [Int], [Chunk], [Tag])
parseMCA = parseOnly toplevel

-- NBT format is for describing blocks and such
parseNBT :: B.ByteString -> Either String Tag
parseNBT = parseOnly nbt

toplevel :: Parser ([Location], [Int], [Chunk], [Tag])
toplevel = do
  ls <- count 1024 location
  ts <- count 1024 timestamp
  cs <- many1 chunk
  tags <- case mapM parseNBT cs of
    (Right tags') -> return tags'
    (Left msg) -> error msg
  return (ls, ts, cs, tags)

location :: Parser Location
location = do
  loc <- unsigned_int 3
  sectors <- unsigned_int 1
  return (loc, sectors)

timestamp :: Parser Int
timestamp = unsigned_int 4

chunk :: Parser Chunk
chunk = do
  -- the size of the compressed data black + 1
  chunkLength <- unsigned_int 4

  -- the compression format
  compression <- word8 0x01 <|> word8 0x02

  -- minus 1 since the compression type byte is removed
  compressedChunk <- take (chunkLength - 1)

  -- the entire block needs to be a multiple of 4096, this includes the
  -- chunkLength AND the 4 size bytes
  padding <- case mod (chunkLength + 4) 4096 of
    0 -> take 0
    i -> take (4096 - i)

  return $ decompress compressedChunk

decompress :: B.ByteString -> B.ByteString
decompress = BL.toStrict . Zlib.decompress . BL.fromStrict

nbt :: Parser Tag
nbt = tag Nothing tagName

-- showTag :: Tag -> B.ByteString
-- showTag (TagByte n word)        = "TAG_Byte("      <> n <> ")" <> hexByte word
-- showTag (TagShort n x)          = "TAG_Short("     <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagInt n x)            = "TAG_Int("       <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagLong n x)           = "TAG_Long("      <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagFloat n x)          = "TAG_Float("     <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagDouble n x)         = "TAG_Double("    <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagByteArray n s)      = "TAG_ByteArray(" <> n <> ")" <> "[" <> toHex s <> "]"
-- showTag (TagString n s)         = "TAG_String("    <> n <> ")" <> "\"" <> s <> "\""
-- showTag (TagList n xs)          = "TAG_List("      <> n <> ")" <> ("[" <> B.intercalate ", " (map showTag xs) <> "]")
-- showTag (TagCompound n entries) = "TAG_Compound("  <> n <> ")" <> ("{" <> B.intercalate ", " (map showTag entries) <> "}")
-- showTag (TagIntArray n xs)      = "TAG_IntArray("  <> n <> ("[" <> B.intercalate ", " [ BSU.fromString $ show x | x <- xs] <> "]")
-- showTag (TagLongArray n xs)     = "TAG_LongArray(" <> n <> ("[" <> B.intercalate ", " [ BSU.fromString $ show x | x <- xs] <> "]")
-- showTag (TagEnd)                = error "TagEnd should not appear in output"

addName :: Name -> B.ByteString -> B.ByteString
addName "" x = x 
addName name x = x <> "(" <> name <> ")" 


toHex :: B.ByteString -> B.ByteString
toHex xs = B.concatMap hexByte xs

hexByte :: Word8 -> B.ByteString
hexByte x =  hexa (fromIntegral (shiftR x 4)) <> hexa (fromIntegral (shiftR (shiftL x 4) 4)) where
  hexa 0x0 = "0"
  hexa 0x1 = "1"
  hexa 0x2 = "2"
  hexa 0x3 = "3"
  hexa 0x4 = "4"
  hexa 0x5 = "5"
  hexa 0x6 = "6"
  hexa 0x7 = "7"
  hexa 0x8 = "8"
  hexa 0x9 = "9"
  hexa 0xa = "a"
  hexa 0xb = "b"
  hexa 0xc = "c"
  hexa 0xd = "d"
  hexa 0xe = "e"
  hexa 0xf = "f"


tag :: Maybe TagId -> Parser Name -> Parser Tag
tag tagid namer = do
  id <- case tagid of
    (Just id') -> return id'
    Nothing -> do
      code <- anyWord8
      case code2tagid code of
        (Just tagid') -> return tagid'
        Nothing -> error "Illegal tag code"
  tagParser id
  where
    tagParser :: TagId -> Parser Tag 
    tagParser TagEndId       = tag_end
    tagParser TagByteId      = tag_byte namer
    tagParser TagShortId     = tag_short namer
    tagParser TagIntId       = tag_int namer
    tagParser TagLongId      = tag_long namer
    tagParser TagFloatId     = tag_float namer
    tagParser TagDoubleId    = tag_double namer
    tagParser TagByteArrayId = tag_byte_array namer
    tagParser TagStringId    = tag_string namer
    tagParser TagListId      = tag_list namer
    tagParser TagCompoundId  = tag_compound namer
    tagParser TagIntArrayId  = tag_int_array namer
    tagParser TagLongArrayId = tag_long_array namer

tagName :: Parser Name
tagName = do
  size <- unsigned_int 2
  name <- take size
  return name

emptyName :: Parser Name
emptyName = return ""


-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 0  NAME: TAG_End
-- Payload: None.
-- Note: This tag is used to mark the end of a list.  Cannot be named!
--       If type 0 appears where a Named Tag is expected, the name is assumed
--       to be "".  (In other words, this Tag is always just a single 0 byte
--       when named, and nothing in all other cases)
tag_end :: Parser Tag
tag_end = do
  -- _ <- word8 0x00
  return TagEnd

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 1  NAME: TAG_Byte
-- Payload: A single signed byte (8 bits)
tag_byte :: Parser Name -> Parser Tag
tag_byte namer = do
  -- _ <- word8 0x01
  name <- namer
  x <- B.head <$> take 1
  return $ TagByte name x

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 2  NAME: TAG_Short
-- Payload: A signed short (16 bits, big endian)
tag_short :: Parser Name -> Parser Tag
tag_short namer = do
  -- _ <- word8 0x02
  name <- namer
  i <- signed_int 2
  return $ TagShort name i

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 3  NAME: TAG_Int
-- Payload: A signed short (32 bits, big endian)
tag_int :: Parser Name -> Parser Tag
tag_int namer = do
  -- _ <- word8 0x03
  name <- namer
  i <- signed_int 4
  return $ TagShort name i

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 4  NAME: TAG_Long
-- Payload: A signed long (64 bits, big endian)
tag_long :: Parser Name -> Parser Tag
tag_long namer = do
  -- _ <- word8 0x04
  name <- namer
  i <- signed_int 8
  return $ TagLong name i

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 5  NAME: TAG_Float
-- Payload: A floating point value (32 bits, big endian, IEEE 754-2008, binary32)
tag_float :: Parser Name -> Parser Tag
tag_float namer = do
  -- _ <- word8 0x05
  name <- namer
  x <- float
  return $ TagFloat name x

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 6  NAME: TAG_Double
-- Payload: A floating point value (64 bits, big endian, IEEE 754-2008, binary64)
tag_double :: Parser Name -> Parser Tag
tag_double namer = do
  -- _ <- word8 0x06
  name <- namer
  x <- double
  return $ TagDouble name x

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 7  NAME: TAG_Byte_Array
-- Payload: TAG_Int length
--          An array of bytes of unspecified format. The length of this array is <length> bytes
tag_byte_array :: Parser Name -> Parser Tag
tag_byte_array namer = do
  -- _ <- word8 0x07
  name <- namer
  size <- unsigned_int 4
  array <- take size
  return $ TagByteArray name array

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 8  NAME: TAG_String
-- Payload: TAG_Short length
--          An array of bytes defining a string in UTF-8 format. The length of this array is <length> bytes
tag_string :: Parser Name -> Parser Tag
tag_string namer = do
  -- _ <- word8 0x08
  name <- namer
  size <- unsigned_int 2
  string <- take size
  return $ TagString name string

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 9  NAME: TAG_List
-- Payload: TAG_Byte tagId
--          TAG_Int length
--          A sequential list of Tags (not Named Tags), of type <typeId>.
--          The length of this array is <length> Tags
-- Notes:   All tags share the same type.
tag_list :: Parser Name -> Parser Tag
tag_list namer = do
  -- _ <- word8 0x09
  name <- namer
  tagcode <- anyWord8
  size <- unsigned_int 4
  tagid <- case code2tagid tagcode of
    (Just tagid') -> return tagid'
    Nothing -> error "Illegal code"
  tags <- count size $ tag (Just tagid) emptyName
  return $ TagList size tagid name tags

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 10 NAME: TAG_Compound
-- Payload: A sequential list of Named Tags. This array keeps going until a TAG_End is found.
--          TAG_End end
-- Notes:   If there's a nested TAG_Compound within this tag, that one will also
--          have a TAG_End, so simply reading until the next TAG_End will not work.
--          The names of the named tags have to be unique within each TAG_Compound
--          The order of the tags is not guaranteed.
tag_compound :: Parser Name -> Parser Tag
tag_compound namer = do
  -- _ <- word8 0x0a
  name <- namer
  tags <- many' (tag Nothing tagName)
  _ <- tag_end
  return $ TagCompound name tags

-- TAG_Int's payload size, then size TAG_Int's payloads.
tag_int_array :: Parser Name -> Parser Tag
tag_int_array namer = do
  -- _ <- word8 0x0b
  name <- namer
  size <- unsigned_int 4
  array <- count size (signed_int 4)
  return $ TagIntArray name array

-- TAG_Int's payload size, then size TAG_Long's payloads.
tag_long_array :: Parser Name -> Parser Tag
tag_long_array namer = do
  -- _ <- word8 0x0c
  name <- namer
  size <- unsigned_int 4
  array <- count size (signed_int 8)
  return $ TagLongArray name array

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
