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
  , module Data.Word
  , module Data.Word.Word24
  , module Data.Int
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Word.Word24 (Word24)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.ByteString as B

import qualified ParseMinecraft.Parser as P

type SInt8  = Int8 
type SInt16 = Int16 
type SInt32 = Int32
type SInt64 = Int64

type UInt8  = Word8 
type UInt16 = Word16 
type UInt24 = Word24
type UInt32 = Word32
type UInt64 = Word64

type Float32 = Float
type Float64 = Double


type Location = (UInt24, UInt8)

type Timestamp = UInt32

data NamedTag = NamedTag B.ByteString Tag deriving(Show, Ord, Eq)

type Name = B.ByteString

data Tag
  = TagEnd
  | TagByte Name SInt8
  | TagShort Name SInt16
  | TagInt Name SInt32
  | TagLong Name SInt64
  | TagFloat Name Float32
  | TagDouble Name Float64
  | TagByteArray Name B.ByteString
  | TagString Name B.ByteString
  | TagList TagId Name [Tag]
  | TagCompound Name [Tag]
  | TagIntArray Name [SInt32]
  | TagLongArray Name [SInt64]
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

-- A region contains all the data from a single minecraft save file (a given
-- game may have many of these files). A region consists of a 32X32 grid of
-- chunks, where each chunk is a column of 16X16X16 block sections.
data Region = Region
  { xOffset :: SInt32 -- offset from origin
  , zOffset :: SInt32
  , chunks :: [[Chunk]]
  }

-- 16 X 16 X 16 cube of blocks
type Chunk = [Section]

-- A 16X16X16 block cube
type Section = [Block]

-- for now, just the name will suffice
type Block = B.ByteString


code2tagid :: UInt8 -> Maybe TagId
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

tagid2code :: TagId -> UInt8
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
