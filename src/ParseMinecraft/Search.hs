{-# LANGUAGE ScopedTypeVariables #-}

module ParseMinecraft.Search
  ( chunkXZ
  , bitspread
  , bitspreadPadded
  , pointcloud
  ) where

import qualified Data.Maybe as DM
import qualified Data.ByteString as B
import Data.Word (Word64, Word16, Word8)
import Data.Bits (shiftL, shiftR, (.|.), FiniteBits, finiteBitSize)
import qualified Data.Bits as Bits

import ParseMinecraft.Namespace

pointcloud :: [Tag] -> [(B.ByteString, Int64, Int64, Int64)]
pointcloud = concatMap f where
    f (TagCompound "" ts) = concatMap f ts
    f (TagCompound "Level" chunks) = processChunks chunks
    f _ = []


processChunks :: [Tag] -> [(B.ByteString, Int64, Int64, Int64)]
processChunks tags = case seek findSections tags of
    Nothing -> []
    (Just sections) -> concatMap processSection sections


processSection :: Tag -> [(B.ByteString, Int64, Int64, Int64)]
processSection (TagCompound "" tags) = case seekTwo findBlockStates findPaletteNames tags of
    Nothing -> []
    (Just (packedIndices, palette)) ->
        let size = ceiling (logBase 2 (fromIntegral (length palette)))
            indices = bitspread size packedIndices
        in processCube indices palette
processSection _ = error "Unexpected section format"


processCube :: [Int] -> [B.ByteString] -> [(B.ByteString, Int64, Int64, Int64)]
processCube indices names = zipWith f [names !! i' | i' <- indices] [0..] where
    f :: B.ByteString -> Int64 -> (B.ByteString, Int64, Int64, Int64)
    f x i = (x, mod i 16, mod (div i 16) (16*16), div i (16*16))


cubes :: (FiniteBits i, Integral i) 
      => (Int, Int, Int)
      -> [i] -- indices into [a]
      -> [a] -- descriptions of each block
      -> [[[a]]] -- folded 3d cube
cubes dimension longs names = foldDimensions dimension [names !! i | i <- bitspread (length names) longs]


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


-- Find the first instance of a pattern
seek :: (b -> Maybe a) -> [b] -> Maybe a
seek _ [] = Nothing
seek f (x:xs) = case f x of
  (Just y) -> Just y
  Nothing -> seek f xs


seekTwo :: (a -> Maybe b) -> (a -> Maybe c) -> [a] -> Maybe (b, c)
seekTwo f g xs = (,) <$> seek f xs <*> seek g xs


findShortByName :: Name -> Tag -> Maybe Int16
findShortByName name (TagShort tagName x)
  | tagName == name = Just x
  | otherwise = Nothing
findShortByName name (TagCompound _ xs) = seek (findShortByName name) xs
findShortByName name (TagList TagShortId _ xs) = seek (findShortByName name) xs
findShortByName _ _ = Nothing


xPos :: Tag -> Maybe Int16
xPos = findShortByName "xPos"


zPos :: Tag -> Maybe Int16
zPos = findShortByName "zPos"


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

        -- Int is the number of unused bits left in the current b value
        -- [b] is the input data we are re-partioning
        -- h :: (FiniteBits b, FiniteBits c) => Int -> [b] -> [c]
        h _ [] = []
        h 0 (_:xs) = h nb xs
        h k (x:xs)
            -- there are enough bits (this may leave k bits as 0)
            | n <= k = fromIntegral (shiftR (shiftL x (nb - k)) (nb - n)) : h (k - n) (x:xs)
            -- split
            | otherwise = case xs of
                -- if there are not enough bits left to fill y, then y is padding, return nothing
                [] -> []
                -- otherwise grab bits from both x and the next long x'
                (x':xs') ->
                    let y = shiftR (shiftL x (nb - k)) (nb - n) .|. shiftR x' (nb - n + k)
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

        h :: (FiniteBits b, FiniteBits c, Integral b, Integral c) => Int -> [b] -> [c]
        h _ [] = []
        h offset (x:xs)
            | nb - offset < n = h 0 xs
            | otherwise = fromIntegral (shiftR (shiftL x (fromIntegral offset)) (nb - nc)) : h (offset + n) (x:xs)


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

findBlockStates :: Tag -> Maybe [Int64]
findBlockStates (TagCompound "" xs) = seek findBlockStates xs
findBlockStates (TagLongArray "BlockStates" xs) = Just xs
findBlockStates _ = Nothing


findPaletteNames :: Tag -> Maybe [B.ByteString]
findPaletteNames (TagCompound "" xs) = seek findPaletteNames xs
findPaletteNames (TagList _ "Palette" xs) = mapM inCompound xs
  where
    inCompound :: Tag -> Maybe B.ByteString
    inCompound (TagCompound "" ys) = seek findName ys
    inCompound _ = Nothing
findPaletteNames _ = Nothing


findSections :: Tag -> Maybe [Tag]
findSections (TagList _ "Sections" xs) = Just xs
findSections _ = Nothing


findName :: Tag -> Maybe B.ByteString
findName (TagString "name" findName) = Just findName
findName _ = Nothing
