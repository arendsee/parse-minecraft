module Numbers
  ( signed_int 
  , unsigned_int
  , float
  , double
  ) where

import Prelude hiding (take)
import qualified Data.Bits as B
import Data.Bits ((.|.))
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Attoparsec.ByteString (Parser, anyWord8, count, take)
import qualified Data.ByteString as BS

-- IEEE float:
-- bit 31 : sign
-- bit 30-23 (8 bits) : exponent
-- bit 22-0 (23 bits) : significand
float :: Parser (Bool, Int, Int)
float = do
  words <- fmap BS.unpack $ take 4
  return $ word2float words


word2float :: [Word8] -> (Bool, Int, Int)
word2float [w0,w1,w2,w3] =
  let
    sign = B.testBit w0 7

    -- take the last bit of the second byte and the first seven bits of the first word
    exponent = unsignedBigEndianInt [B.rotateL w1 1 .|. B.rotateR w2 7]

    significand = unsignedBigEndianInt [w1,w2,w3]

  in (sign, exponent, significand)
word2float _ = error "expected 4 words"

double :: Parser (Bool, Int, Int) 
double = do
  words <- fmap BS.unpack $ take 8
  return $ word2double words

word2double :: [Word8] -> (Bool, Int, Int)
word2double [w0,w1,w2,w3,w4,w5,w6,w7] =
  let
    --   byte 0   byte 1   byte 2   byte 3   byte 4   byte 5   byte 6   byte 7  
    --  +.......|....****|********|********|********|********|********|********|
    --  ^  exponent               significand
    --  |
    --   '- sign

    sign = B.testBit w0 7

    exponent = unsignedBigEndianInt
      [ B.rotateR (B.rotateL w1 1) 4  -- rotate left to erase sign, then right to erase bits in second exp word
      , B.rotateR w2 4 .|. B.rotateL w1 4
      ]

    significand = unsignedBigEndianInt [B.rotateR (B.rotateL w1 4) 4,w2,w3,w4,w5,w6,w7]

  in (sign, exponent, significand)
word2double _ = error "expected 4 words"

-- take i bytes and turn it into an unsigned int
unsigned_int :: Int -> Parser Int
unsigned_int i = fmap unsignedBigEndianInt $ count i anyWord8

signed_int :: Int -> Parser Int
signed_int i = fmap (sign . unsignedBigEndianInt) $ count i anyWord8 
  where
    sign :: Int -> Int
    sign i
      | B.testBit i 63 = B.complement i + 1
      | otherwise = i

unsignedBigEndianInt :: [Word8] -> Int
unsignedBigEndianInt xs = sum $ zipWith (\l x -> B.shiftL (fromIntegral x) l) [0,8..] (reverse xs)
