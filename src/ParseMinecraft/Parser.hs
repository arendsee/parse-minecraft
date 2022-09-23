-- This module is adapted (plagiarized) from Data.Attoparsec.Binary
-- That module contained a few deprecated functions and was quite short anyway, so it is easier to rewrite it here
-- Specifically:
--   * use the new FiniteBits class from Data.Bits to represent fixed size data (which is all I care about here)

module ParseMinecraft.Parser
  (
  -- unsigned ints
    anyUInt8be
  , anyUInt8le
  , anyUInt16be
  , anyUInt16le
  , anyUInt24be
  , anyUInt24le
  , anyUInt32be
  , anyUInt32le
  , anyUInt64be
  , anyUInt64le
  , uInt8be
  , uInt8le
  , uInt16be
  , uInt16le
  , uInt24be
  , uInt24le
  , uInt32be
  , uInt32le
  , uInt64be
  , uInt64le
  -- signed ints
  , anySInt8be
  , anySInt8le
  , anySInt16be
  , anySInt16le
  , anySInt32be
  , anySInt32le
  , anySInt64be
  , anySInt64le
  , sInt8be
  , sInt8le
  , sInt16be
  , sInt16le
  , sInt32be
  , sInt32le
  , sInt64be
  , sInt64le
  -- floats
  , anyFloat32le
  , anyFloat32be
  , anyFloat64le
  , anyFloat64be
  , float32le
  , float32be
  , float64le
  , float64be
  -- other
  , unsignedInt
  , signedInt
  , unsignedBigEndianInt
  ) where


import Prelude hiding (take)
import Data.Attoparsec.ByteString
import Data.Bits
import Data.Int
import Data.Word
import Data.Word.Word24
import GHC.Float (castWord32ToFloat, castWord64ToDouble, castFloatToWord32, castDoubleToWord64)
import qualified Data.ByteString as BS

byteSize :: (FiniteBits a) => a -> Int
byteSize = (`div` 8) . finiteBitSize

-- If the bytestring has more bytes than the object, than just the last bytes will be included
pack :: (FiniteBits a, Num a) => BS.ByteString -> a
pack = BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

unpack :: (FiniteBits a, Integral a) => a -> BS.ByteString
unpack x = BS.pack $ map f $ reverse [0..byteSize x - 1]
  where f s = fromIntegral $ shiftR x (8 * s)

anyN :: (FiniteBits a) => (BS.ByteString -> a) -> Parser a
anyN = anyN' undefined
  -- The undefined term here is used just to pass type information to
  -- `byteSize`. Since the value is never accessed no error is raised.
  where anyN' :: (FiniteBits a) => a -> (BS.ByteString -> a) -> Parser a
        anyN' d = flip fmap $ take $ byteSize d

bitsN :: (Bits a) => (a -> BS.ByteString) -> a -> Parser a
bitsN u w = string (u w) >> return w

anyUInt8be :: Parser Word8
anyUInt8be = anyN pack

anyUInt8le :: Parser Word8
anyUInt8le = anyN pack

anyUInt16be :: Parser Word16
anyUInt16be = anyN pack

anyUInt16le :: Parser Word16
anyUInt16le = anyN $ pack . BS.reverse

anyUInt24be :: Parser Word24
anyUInt24be = anyN pack

anyUInt24le :: Parser Word24
anyUInt24le = anyN $ pack . BS.reverse

anyUInt32be :: Parser Word32
anyUInt32be = anyN pack

anyUInt32le :: Parser Word32
anyUInt32le = anyN $ pack . BS.reverse

anyUInt64be :: Parser Word64
anyUInt64be = anyN pack

anyUInt64le :: Parser Word64
anyUInt64le = anyN $ pack . BS.reverse


uInt8be :: Word8 -> Parser Word8
uInt8be = bitsN unpack

uInt8le :: Word8 -> Parser Word8
uInt8le = bitsN unpack

uInt16be :: Word16 -> Parser Word16
uInt16be = bitsN unpack

uInt16le :: Word16 -> Parser Word16
uInt16le = bitsN $ BS.reverse . unpack

uInt24be :: Word24 -> Parser Word24
uInt24be = bitsN unpack

uInt24le :: Word24 -> Parser Word24
uInt24le = bitsN $ BS.reverse . unpack

uInt32be :: Word32 -> Parser Word32
uInt32be = bitsN unpack

uInt32le :: Word32 -> Parser Word32
uInt32le = bitsN $ BS.reverse . unpack

uInt64be :: Word64 -> Parser Word64
uInt64be = bitsN unpack

uInt64le :: Word64 -> Parser Word64
uInt64le = bitsN $ BS.reverse . unpack



anySInt8be :: Parser Int8
anySInt8be = anyN pack

anySInt8le :: Parser Int8
anySInt8le = anyN pack -- just one byte, no need to reverse

anySInt16be :: Parser Int16
anySInt16be = anyN pack

anySInt16le :: Parser Int16
anySInt16le = anyN $ pack . BS.reverse

anySInt32be :: Parser Int32
anySInt32be = anyN pack

anySInt32le :: Parser Int32
anySInt32le = anyN $ pack . BS.reverse

anySInt64be :: Parser Int64
anySInt64be = anyN pack

anySInt64le :: Parser Int64
anySInt64le = anyN $ pack . BS.reverse


sInt8be :: Int8 -> Parser Int8
sInt8be = bitsN unpack

sInt8le :: Int8 -> Parser Int8
sInt8le = bitsN unpack -- just one byte, no need to reverse

sInt16be :: Int16 -> Parser Int16
sInt16be = bitsN unpack

sInt16le :: Int16 -> Parser Int16
sInt16le = bitsN $ BS.reverse . unpack

sInt32be :: Int32 -> Parser Int32
sInt32be = bitsN unpack

sInt32le :: Int32 -> Parser Int32
sInt32le = bitsN $ BS.reverse . unpack

sInt64be :: Int64 -> Parser Int64
sInt64be = bitsN unpack

sInt64le :: Int64 -> Parser Int64
sInt64le = bitsN $ BS.reverse . unpack



anyFloat32be :: Parser Float
anyFloat32be = castWord32ToFloat <$> anyUInt32be

anyFloat32le :: Parser Float
anyFloat32le = castWord32ToFloat <$> anyUInt32le

anyFloat64be :: Parser Double
anyFloat64be = castWord64ToDouble <$> anyUInt64be

anyFloat64le :: Parser Double
anyFloat64le = castWord64ToDouble <$> anyUInt64le
    

float32be :: Float -> Parser Float
float32be x = uInt32be (castFloatToWord32 x) >> return x

float32le :: Float -> Parser Float
float32le x = uInt32le (castFloatToWord32 x) >> return x

float64be :: Double -> Parser Double
float64be x = uInt64be (castDoubleToWord64 x) >> return x

float64le :: Double -> Parser Double
float64le x = uInt64le (castDoubleToWord64 x) >> return x



-- take i bytes and turn it into an unsigned int
unsignedInt :: Int -> Parser Int
unsignedInt i = unsignedBigEndianInt <$> count i anyUInt8be

signedInt :: Int -> Parser Int
signedInt i = sign . unsignedBigEndianInt <$> count i anyUInt8be
  where
    sign :: Int -> Int
    sign i
      | testBit i 63 = complement i + 1
      | otherwise = i

unsignedBigEndianInt :: [Word8] -> Int
unsignedBigEndianInt xs = sum $ zipWith (\l x -> shiftL (fromIntegral x) l) [0,8..] (reverse xs)




-- -- IEEE float:
-- -- bit 31 : sign
-- -- bit 30-23 (8 bits) : exponent
-- -- bit 22-0 (23 bits) : significand
-- float :: Parser (Bool, Int, Int)
-- float = do
--   words <- BS.unpack <$> take 4
--   return $ word2float words
--
--
-- word2float :: [Word8] -> (Bool, Int, Int)
-- word2float [w0,w1,w2,w3] =
--   let
--     sign = testBit w0 7
--
--     -- take the last bit of the second byte and the first seven bits of the first word
--     exponent = unsignedBigEndianInt [shiftL w0 1 .|. shiftR w1 7]
--
--     significand = unsignedBigEndianInt [clearBit w1 7, w2, w3]
--
--   in (sign, exponent, significand)
-- word2float _ = error "expected 4 words"
--
-- double :: Parser (Bool, Int, Int)
-- double = do
--   words <- BS.unpack <$> take 8
--   return $ word2double words
--
-- word2double :: [Word8] -> (Bool, Int, Int)
-- word2double [w0,w1,w2,w3,w4,w5,w6,w7] =
--   let
--     --   byte 0   byte 1   byte 2   byte 3   byte 4   byte 5   byte 6   byte 7
--     --  +.......|....****|********|********|********|********|********|********|
--     --  ^  exponent               significand
--     --  |
--     --   '- sign
--
--     sign = testBit w0 7
--
--     exponent = unsignedBigEndianInt
--       [ shiftL w0 1 .|. shiftR w1 7
--       , shiftR (clearBit w1 7) 4
--       ]
--
--     significand = unsignedBigEndianInt [shiftR (shiftL w1 4) 4, w2, w3, w4, w5, w6, w7]
--
--   in (sign, exponent, significand)
-- word2double _ = error "expected 4 words"
