module ParseMinecraft.Namespace
  ( Tag(..)
  , TagId(..)
  , Chunk(..)
  , Location
  , Timestamp
  , NamedTag
  , Name
  , code2tagid
  , tagid2code
  ) where

import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as B

type Location = (Int, Int)

type Timestamp = Word32

data NamedTag = NamedTag B.ByteString Tag deriving(Show, Ord, Eq)

type Name = B.ByteString

data Tag
  = TagEnd
  | TagByte Name Word8
  | TagShort Name Int
  | TagInt Name Int
  | TagLong Name Int
  | TagFloat Name (Bool, Int, Int)
  | TagDouble Name (Bool, Int, Int)
  | TagByteArray Name B.ByteString
  | TagString Name B.ByteString
  | TagList Int TagId Name [Tag]
  | TagCompound Name [Tag]
  | TagIntArray Name [Int]
  | TagLongArray Name [Int]
  deriving(Show, Ord, Eq)

data TagId
  = TagEndId
  | TagByteId
  | TagShortId
  | TagIntId
  | TagLongId
  | TagFloatId
  | TagDoubleId
  | TagByteArrayId
  | TagStringId
  | TagListId
  | TagCompoundId
  | TagIntArrayId
  | TagLongArrayId
  deriving(Show, Ord, Eq)

data Chunk = Chunk
  { xOffset :: Int
  , zOffset :: Int
  , blocks :: [[[B.ByteString]]]
  }


code2tagid :: Word8 -> Maybe TagId
code2tagid w = case w of
    0x00 -> Just TagEndId
    0x01 -> Just TagByteId
    0x02 -> Just TagShortId
    0x03 -> Just TagIntId
    0x04 -> Just TagLongId
    0x05 -> Just TagFloatId
    0x06 -> Just TagDoubleId
    0x07 -> Just TagByteArrayId
    0x08 -> Just TagStringId
    0x09 -> Just TagListId
    0x0a -> Just TagCompoundId
    0x0b -> Just TagIntArrayId
    0x0c -> Just TagLongArrayId
    _ -> Nothing

tagid2code :: TagId -> Word8
tagid2code TagEndId       = 0x00 
tagid2code TagByteId      = 0x01 
tagid2code TagShortId     = 0x02 
tagid2code TagIntId       = 0x03 
tagid2code TagLongId      = 0x04 
tagid2code TagFloatId     = 0x05 
tagid2code TagDoubleId    = 0x06 
tagid2code TagByteArrayId = 0x07 
tagid2code TagStringId    = 0x08 
tagid2code TagListId      = 0x09 
tagid2code TagCompoundId  = 0x0a 
tagid2code TagIntArrayId  = 0x0b 
tagid2code TagLongArrayId = 0x0c 
