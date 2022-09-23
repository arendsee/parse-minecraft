module ParseMinecraft.ParseMCA (parseMCA, hexByte) where

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

import qualified ParseMinecraft.Parser as P
import ParseMinecraft.Namespace
import ParseMinecraft.Parser

type ChunkData = B.ByteString

-- MCA format is for descirbing 32x32 chunk regions
parseMCA :: B.ByteString -> Either String ([Location], [Int], [ChunkData], [Tag])
parseMCA = parseOnly toplevel

-- NBT format is for describing blocks and such
parseNBT :: B.ByteString -> Either String Tag
parseNBT = parseOnly nbt

toplevel :: Parser ([Location], [Int], [ChunkData], [Tag])
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
  loc <- P.anyUInt24be
  sectors <- P.anyUInt8be
  return (loc, sectors)

timestamp :: Parser Int
timestamp = unsignedInt 4

chunk :: Parser ChunkData
chunk = do
  -- the size of the compressed data black + 1
  chunkLength <- unsignedInt 4

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
-- showTag (TagByte n word)        = "TagByte("      <> n <> ")" <> hexByte word
-- showTag (TagShort n x)          = "TagShort("     <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagInt n x)            = "TagInt("       <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagLong n x)           = "TagLong("      <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagFloat n x)          = "TagFloat("     <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagDouble n x)         = "TagDouble("    <> n <> ")" <> BSU.fromString (show x)
-- showTag (TagByteArray n s)      = "TagByteArray(" <> n <> ")" <> "[" <> toHex s <> "]"
-- showTag (TagString n s)         = "TagString("    <> n <> ")" <> "\"" <> s <> "\""
-- showTag (TagList n xs)          = "TagList("      <> n <> ")" <> ("[" <> B.intercalate ", " (map showTag xs) <> "]")
-- showTag (TagCompound n entries) = "TagCompound("  <> n <> ")" <> ("{" <> B.intercalate ", " (map showTag entries) <> "}")
-- showTag (TagIntArray n xs)      = "TagIntArray("  <> n <> ("[" <> B.intercalate ", " [ BSU.fromString $ show x | x <- xs] <> "]")
-- showTag (TagLongArray n xs)     = "TagLongArray(" <> n <> ("[" <> B.intercalate ", " [ BSU.fromString $ show x | x <- xs] <> "]")
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
  case id of
    TagByteId      -> tagByte namer
    TagShortId     -> tagShort namer
    TagIntId       -> tagInt namer
    TagLongId      -> tagLong namer
    TagFloatId     -> tagFloat namer
    TagDoubleId    -> tagDouble namer
    TagByteArrayId -> tagByteArray namer
    TagStringId    -> tagString namer
    TagListId      -> tagList namer
    TagCompoundId  -> tagCompound namer
    TagIntArrayId  -> tagIntArray namer
    TagLongArrayId -> tagLongArray namer
    TagEndId       -> tagEnd

tagName :: Parser Name
tagName = do
  size <- unsignedInt 2
  take size

emptyName :: Parser Name
emptyName = return ""


-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 0  NAME: TAGEnd
-- Payload: None.
-- Note: This tag is used to mark the end of a list.  Cannot be named!
--       If type 0 appears where a Named Tag is expected, the name is assumed
--       to be "".  (In other words, this Tag is always just a single 0 byte
--       when named, and nothing in all other cases)
tagEnd :: Parser Tag
tagEnd = return TagEnd

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 1  NAME: TAG_Byte
-- Payload: A single signed byte (8 bits)
tagByte :: Parser Name -> Parser Tag
tagByte namer = TagByte <$> namer <*> anySInt8be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 2  NAME: TAG_Short
-- Payload: A signed short (16 bits, big endian)
tagShort :: Parser Name -> Parser Tag
tagShort namer = TagShort <$> namer <*> anySInt16be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 3  NAME: TAG_Int
-- Payload: A signed short (32 bits, big endian)
tagInt :: Parser Name -> Parser Tag
tagInt namer = TagInt <$> namer <*> anySInt32be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 4  NAME: TAG_Long
-- Payload: A signed long (64 bits, big endian)
tagLong :: Parser Name -> Parser Tag
tagLong namer = TagLong <$> namer <*> P.anySInt64be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 5  NAME: TAG_Float
-- Payload: A floating point value (32 bits, big endian, IEEE 754-2008, binary32)
tagFloat :: Parser Name -> Parser Tag
tagFloat namer = TagFloat <$> namer <*> P.anyFloat32be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 6  NAME: TAG_Double
-- Payload: A floating point value (64 bits, big endian, IEEE 754-2008, binary64)
tagDouble :: Parser Name -> Parser Tag
tagDouble namer = TagDouble <$> namer <*> P.anyFloat64be

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 7  NAME: TAG_Byte_Array
-- Payload: TAG_Int length
--          An array of bytes of unspecified format. The length of this array is <length> bytes
tagByteArray :: Parser Name -> Parser Tag
tagByteArray namer = do
  name <- namer
  size <- unsignedInt 4
  array <- take size
  return $ TagByteArray name array

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 8  NAME: TAG_String
-- Payload: TAG_Short length
--          An array of bytes defining a string in UTF-8 format. The length of this array is <length> bytes
tagString :: Parser Name -> Parser Tag
tagString namer = do
  name <- namer
  size <- unsignedInt 2
  string <- take size
  return $ TagString name string

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 9  NAME: TAG_List
-- Payload: TAG_Byte tagId
--          TAG_Int length
--          A sequential list of Tags (not Named Tags), of type <typeId>.
--          The length of this array is <length> Tags
-- Notes:   All tags share the same type.
tagList :: Parser Name -> Parser Tag
tagList namer = do
  name <- namer
  tagcode <- anyWord8
  size <- unsignedInt 4
  tagid <- case code2tagid tagcode of
    (Just tagid') -> return tagid'
    Nothing -> error "Illegal code"
  tags <- count size $ tag (Just tagid) emptyName
  return $ TagList tagid name tags

-- -- from http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- TYPE: 10 NAME: TAG_Compound
-- Payload: A sequential list of Named Tags. This array keeps going until a TAG_End is found.
--          TAG_End end
-- Notes:   If there's a nested TAG_Compound within this tag, that one will also
--          have a TAG_End, so simply reading until the next TAG_End will not work.
--          The names of the named tags have to be unique within each TAG_Compound
--          The order of the tags is not guaranteed.
tagCompound :: Parser Name -> Parser Tag
tagCompound namer = do
  name <- namer
  tags <- manyTill (tag Nothing tagName) (word8 0x0)
  return $ TagCompound name tags

-- TAG_Int's payload size, then size TAG_Int's payloads.
tagIntArray :: Parser Name -> Parser Tag
tagIntArray namer = do
  name <- namer
  size <- unsignedInt 4
  array <- count size anySInt32be
  return $ TagIntArray name array

-- TAG_Int's payload size, then size TAG_Long's payloads.
tagLongArray :: Parser Name -> Parser Tag
tagLongArray namer = do
  name <- namer
  size <- unsignedInt 4
  array <- count size anySInt64be
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
