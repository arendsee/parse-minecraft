module ParseMinecraft.Search
  ( chunkXZ
  , bitspread
  , bitspreadPadded
  ) where

import qualified Data.Maybe as DM
import qualified Data.ByteString as B
import Data.Word (Word64, Word16, Word8)
import Data.Bits (shiftL, shiftR, (.|.))

import ParseMinecraft.Namespace

pointcloud :: [Tag] -> [(B.ByteString, Int64, Int64, Int64)]
pointcloud [] = []
pointcloud [TagCompound "" ts] = pointcloud ts 
pointcloud (TagCompound "Level" chunks : ts) = processSection chunks <> pointcloud ts 
pointcloud _ = []

processSection :: [Tag] -> [(B.ByteString, Int64, Int64, Int64)]
processSection tags = case seekTwo blockStates paletteNames tags of
    Nothing -> []
    (Just (indices, palette)) ->
        let size = ceiling (logBase 2 (fromIntegral (length palette)))
        in processCube (bitspread size indices) palette



processCube :: [Int] -> [B.ByteString] -> [(B.ByteString, Int64, Int64, Int64)]
processCube indices names = zipWith f [names !! i' | i' <- indices] [0..] where
    f :: B.ByteString -> Int64 -> (B.ByteString, Int64, Int64, Int64)
    f x i = (x, mod i 16, mod (div i 16) (16*16), div i (16*16))



cubes :: (Integral i) 
      => (Int, Int, Int)
      -> [i] -- indices into [a]
      -> [a] -- descriptions of each block
      -> [[[a]]] -- folded 3d cube
cubes dimension longs names = foldDimensions dimension [names !! i | i <- bitspread (fromIntegral $ length names) (map fromIntegral longs)]

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

-- Take size bits at a time from a list of longs. Cast these bits into an
-- output index. Allow indices to cross long boudaries. This function could be
-- generalized considerably.
bitspread
    :: Int -- ^ n: The number of bits in each output index
    -> [Int64] -- ^ xs: A list of longs that encode smaller indices 
    -> [Int] -- ^ ys: final indices, length should be greater than or equal to xs (will only require a Word16
bitspread n = f 64 where

    f :: Int -> [Int64] -> [Int]
    f _ [] = []
    f 0  (_:xs)= f 64 xs
    -- k is the number of bits available in the long
    f k (x:xs)
        -- there are enough bits (this may leave k bits as 0)
        | n <= k = fromIntegral (shiftR (shiftL x (64 - k)) (64 - n)) : f (k - n) (x:xs)
        -- split
        | otherwise = case xs of
            -- if there are not enough bits left to fill y, then y is padding, return nothing
            [] -> []
            -- otherwise grab bits from both x and the next long x'
            (x':xs') ->
                let y = fromIntegral $ shiftR (shiftL x (64 - k)) (64 - n) .|. shiftR x' (64 - n + k) 
                in y : f (64 - n + k) (x':xs')

bitspreadPadded :: Word8 -> [Word64] -> [Word16]
bitspreadPadded size = f 0 where
    size' = fromIntegral size :: Int
    f _ [] = []
    f offset (x:xs)
        | 64 - offset < size = f 0 xs
        | otherwise = fromIntegral (shiftR (shiftL x (fromIntegral offset)) (64 - size')) : f (offset + size) (x:xs)

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

blockStates :: Tag -> Maybe [Int64]
blockStates (TagLongArray "BlockStates" xs) = Just xs
blockStates _ = Nothing

paletteNames :: Tag -> Maybe [B.ByteString]
paletteNames (TagList TagCompoundId "Palette" xs) = mapM inCompound xs
  where
    inCompound :: Tag -> Maybe B.ByteString
    inCompound (TagCompound "" ys) = seek name ys
    inCompound _ = Nothing
paletteNames _ = Nothing

name :: Tag -> Maybe B.ByteString
name (TagString "Name" name) = Just name
name _ = Nothing
