{-# LANGUAGE ScopedTypeVariables #-}

module ParseMinecraft.Search
  ( chunkXZ
  , bitspread
  , bitspreadPadded
  , pointcloud
  , upperMask
  , showBits
  ) where

import qualified Data.Maybe as DM
import qualified Data.ByteString as B
import Data.Word (Word64, Word16, Word8)
import Data.Bits

import ParseMinecraft.Namespace

pointcloud :: [Tag] -> [(B.ByteString, Int64, Int64, Int64)]
pointcloud = concatMap processChunk . concatMap (findTags [TagCompound "" [], TagCompound "Level" []])


processChunk :: Tag -> [(B.ByteString, Int64, Int64, Int64)]
processChunk = concatMap processSection . findTags [TagCompound "Level" [], TagList TagCompoundId "Sections" [], TagCompound "" []]

processSection :: Tag -> [(B.ByteString, Int64, Int64, Int64)]
processSection tag = case (
        findTags [TagCompound "" [], TagLongArray "BlockStates" []] tag,
        findTags [TagCompound "" [], TagList TagCompoundId "Palette" []] tag
    ) of
    ([TagLongArray _ packedIndices], [TagList _ _ palette]) ->
        let size = ceiling (logBase 2 (fromIntegral (length palette)))
            indices = bitspread size packedIndices
            paletteNames = [x | TagString "Name" x <- concatMap (findTags [TagCompound "" [], TagString "Name" ""]) palette]
        in processCube packedIndices indices paletteNames
    _ -> error "Unexpected section format"


{- Performance Bug

Here I am converting from the wrapped longs where block is stored in less than
a single byte to a list where each element includes:
 * 8 byte x
 * 8 byte y
 * 8 byte z
 * 13-27 byte name + 8 byte pointer
 * in a list 8 bytes

I need to work out exactly what the Haskell structures all cost in memory, but
essentially I unpacking from under 1 byte to at over 64 bytes.

I should instead build an unboxed vector of, perhaps, 1 byte indices into a palette.k

-}
processCube :: [Int64] -> [Word64] -> [B.ByteString] -> [(B.ByteString, Int64, Int64, Int64)]
processCube packed indices names = zipWith f [lookupBlock (fromIntegral i') | i' <- indices] [0..] where
  f :: B.ByteString -> Int64 -> (B.ByteString, Int64, Int64, Int64)
  f x i = (x, mod i 16, mod (div i 16) (16*16), div i (16*16))

  n = length names

  lookupBlock :: Int -> B.ByteString
  lookupBlock i
      | i < n = names !! i
      | otherwise = "glitch"


-- cubes :: (FiniteBits i, Integral i)
--       => (Int, Int, Int)
--       -> [i] -- indices into [a]
--       -> [a] -- descriptions of each block
--       -> [[[a]]] -- folded 3d cube
-- cubes dimension longs names = foldDimensions dimension [names !! i | i <- bitspread (length names) longs]


-- | fold a 1D vector of blocks to a 3D vector
foldDimensions
  :: (Int, Int, Int) -- dimensionallity
  -> [a]
  -> [[[a]]]
foldDimensions (x, z, y) xs
  | length folded == y = folded
  | otherwise = error "Incorrect dimension"
  where
      folded = chunksOf z (chunksOf x xs)


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = take i xs : chunksOf i (drop i xs)


findTags :: [Tag] -> Tag -> [Tag]
findTags [] _ = []
findTags [x] y
    | tagsMatch x y = [y]
    | otherwise = []
findTags (TagList t1 n1 _ : patterns) (TagList t2 n2 tags )
    | t1 == t2 && n1 == n2 = concatMap (findTags patterns) tags
    | otherwise = []
findTags (TagCompound n1 _ : patterns) (TagCompound n2 tags)
    | n1 == n2 = concatMap (findTags patterns) tags
    | otherwise = []
-- Nothing other than TagList and TagCompound may contain nested tags
findTags (_:_) _ = []


tagsMatch :: Tag -> Tag -> Bool
tagsMatch TagEnd TagEnd = True
tagsMatch (TagByte       n1 _ ) (TagByte       n2 _ ) = n1 == n2
tagsMatch (TagShort      n1 _ ) (TagShort      n2 _ ) = n1 == n2
tagsMatch (TagInt        n1 _ ) (TagInt        n2 _ ) = n1 == n2
tagsMatch (TagLong       n1 _ ) (TagLong       n2 _ ) = n1 == n2
tagsMatch (TagFloat      n1 _ ) (TagFloat      n2 _ ) = n1 == n2
tagsMatch (TagDouble     n1 _ ) (TagDouble     n2 _ ) = n1 == n2
tagsMatch (TagByteArray  n1 _ ) (TagByteArray  n2 _ ) = n1 == n2
tagsMatch (TagString     n1 _ ) (TagString     n2 _ ) = n1 == n2
tagsMatch (TagIntArray   n1 _ ) (TagIntArray   n2 _ ) = n1 == n2
tagsMatch (TagLongArray  n1 _ ) (TagLongArray  n2 _ ) = n1 == n2
tagsMatch (TagList t1 n1 _) (TagList t2 n2 _ ) = t1 == t2 && n1 == n2
tagsMatch (TagCompound n1 _) (TagCompound n2 _) = n1 == n2
tagsMatch _ _ = False


tagName :: Tag -> B.ByteString
tagName TagEnd = "TagEnd"
tagName (TagByte       n _ ) = n
tagName (TagShort      n _ ) = n
tagName (TagInt        n _ ) = n
tagName (TagLong       n _ ) = n
tagName (TagFloat      n _ ) = n
tagName (TagDouble     n _ ) = n
tagName (TagByteArray  n _ ) = n
tagName (TagString     n _ ) = n
tagName (TagIntArray   n _ ) = n
tagName (TagLongArray  n _ ) = n
tagName (TagList _ n _) = n
tagName (TagCompound n _) = n



xPos :: Tag -> Maybe Int16
xPos = undefined


zPos :: Tag -> Maybe Int16
zPos = undefined


chunkXZ :: Tag -> (Int16, Int16)
chunkXZ = undefined
-- chunkXZ t = case chunkXZsafe t of
--   (Right x) -> x
--   (Left msg) -> error msg


chunkXZsafe :: Tag -> Either String (Int16, Int16)
chunkXZsafe = undefined
-- chunkXZsafe tag = case (xPos tag, zPos tag) of
--   (Just x, Just z) -> Right (x, z)
--   _ -> Left "Missing chunk x or z pos"


chunks :: Tag -> Maybe Chunk
chunks = undefined
-- chunks t = Chunk <$> xPos t <*> zPos t <*> sections t


-- | stack many cubes (e.g., 16X16X16 sectors) into columns on the y-axis
stackBlocksY :: [[[[B.ByteString]]]] -> [[[B.ByteString]]]
stackBlocksY = concat


{- 
with n=5
00001000  00001000  00001000  00001000
.....---  --.....-  ----....  .-----xx
1    0      4    0      16     2

padded
00001000  00001000  00001000  00001000
.....xxx  .....xxx  -----xxx  -----xx
1         1         1         1 

with n=3
padded
00001000  00001000
---...xx  ---...xx
0  2      0  2

with n=3
padded
10001000  10001000
---...xx  ---...xx
4  2      4  2
-}

-- | This seems to behave oddly with signed integers
bitspread
    :: (FiniteBits b, FiniteBits c, Integral b, Integral c)
    => Int -- ^ n: The number of bits in each output index
    -> [b] -- ^ xs: A list of longs that encode smaller indices
    -> [c] -- ^ ys: final indices, length should be greater than or equal to xs (in practice, this will have no more than 16 bits)
bitspread n xs = f undefined undefined xs where
    f :: (FiniteBits b, FiniteBits c, Integral b, Integral c) => b -> c -> [b] -> [c]
    f x y _ = g (finiteBitSize x) (finiteBitSize y)

    -- add the bit sizes to the closure and pass the show to the recursive function h
    g :: (FiniteBits c, Integral c) => Int -> Int -> [c]
    g nb nc = h nb xs where

        mask = upperMask (nb - n)

        -- Int is the number of unused bits left in the current b value
        h _ [] = []
        h 0 (_:xs) = h nb xs
        h k (x:xs)
            -- there are enough bits (this may leave k bits as 0)
            | n <= k = fromIntegral (mask .&. rotateR x (k - n)) : h (k - n) (x:xs)
            -- split
            | otherwise = case xs of
                -- if there are not enough bits left to fill y, then y is padding, return nothing
                [] -> []
                -- otherwise grab bits from both x and the next long x'
                (x':xs') ->
                    let y = mask .&. shiftL x (n - k) .|. shiftR (mask .&. rotateR x' (nb - n)) k
                    in fromIntegral y : h (nb - n + k) (x':xs')



bitspreadPadded
    :: (FiniteBits b, FiniteBits c, Integral b, Integral c)
    => Int -- ^ n: The number of bits in each output index
    -> [b] -- ^ xs: A list of longs that encode smaller indices
    -> [c] -- ^ ys: final indices, length should be greater than or equal to xs (in practice, this will have no more than 16 bits)
bitspreadPadded n xs = f undefined undefined xs where
    f :: (FiniteBits b, FiniteBits c, Integral b, Integral c) => b -> c -> [b] -> [c]
    f x y _ = g (finiteBitSize x) (finiteBitSize y)

    g :: (FiniteBits c, Integral c) => Int -> Int -> [c]
    g nb nc = h nb xs where

        mask = upperMask (nb - n)

        -- h :: (FiniteBits b, FiniteBits c, Integral b, Integral c) => Int -> [b] -> [c]
        h _ [] = []
        h offset (x:xs)
            | offset - n < 0 = h nb xs
            | otherwise = fromIntegral (mask .&. rotateR x (offset - n)) : h (offset - n) (x:xs)


upperMask :: FiniteBits a => Int -> a
upperMask n = f undefined where
    f :: FiniteBits a => a -> a
    f dummy =
        let size = finiteBitSize dummy
        in flip rotateL (size - n) . flip shiftL n . complement $ zeroBits

showBits :: FiniteBits a => a -> String
showBits x = concat [if testBit x i then "1" else "0"
                    | i <- reverse [0 .. (finiteBitSize x - 1)]]

-- -- | A 64 bit long is a long thing, no need to waste space. If the number of
-- -- things being stored only require 32 bits, we might as well store two
-- -- things. If they require 21 bytes, we can store 3. And so on.
-- unpackInt
--   :: Int -- the number of things indexed
--   -> Int -- 64-bit integer storing packed indices
--   -> [Int]  -- unpacked indices
-- unpackInt size x =
--   let nbits = max 4 (ceiling (logBase 2 (fromIntegral size)))
--       k = div 64 nbits -- integer division
--       modder = Bits.shiftL 1 nbits
--   in [Bits.rotateR x ((i-1) * nbits) `mod` modder | i <- [1 .. k]]


-- sections :: Tag -> Maybe [[[B.ByteString]]]
-- sections (TagList TagCompoundId "Sections" xs)
--   = stackBlocksY . map (uncurry (cube (16,16,16))) <$> mapM section xs
-- sections (TagCompound _ xs) = case DM.mapMaybe sections xs of
--   [x] -> Just x
--   _ -> Nothing
-- sections _ = Just []
--
-- section :: Tag -> Maybe ([Int64], [B.ByteString])
-- section = undefined
-- -- section (TagCompound "" xs) = seekTwo blockState paletteNames xs
