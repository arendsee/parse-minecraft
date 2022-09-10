module ParseMinecraft.Search
  ( chunkXZ
  ) where

import qualified Data.Maybe as DM
import qualified Data.ByteString as B
import qualified Data.Bits as Bits

import ParseMinecraft.Namespace

seek :: (b -> Maybe a) -> [b] -> Maybe a
seek _ [] = Nothing
seek f (x:xs) = case f x of
  (Just y) -> Just y
  Nothing -> seek f xs

seekTwo :: (a -> Maybe b) -> (a -> Maybe c) -> [a] -> Maybe (b, c) 
seekTwo f g xs = (,) <$> seek f xs <*> seek g xs  

findShortByName :: Name -> Tag -> Maybe Int
findShortByName name (TagShort tagName x)
  | tagName == name = Just x
  | otherwise = Nothing
findShortByName name (TagCompound _ xs) = seek (findShortByName name) xs
findShortByName name (TagList _ TagShortId _ xs) = seek (findShortByName name) xs
findShortByName _ _ = Nothing

xPos :: Tag -> Maybe Int
xPos = findShortByName "xPos"

zPos :: Tag -> Maybe Int
zPos = findShortByName "zPos"

chunkXZ :: Tag -> (Int, Int)
chunkXZ t = case chunkXZsafe t of
  (Right x) -> x
  (Left msg) -> error msg

chunkXZsafe :: Tag -> Either String (Int, Int)
chunkXZsafe tag = case (xPos tag, zPos tag) of
  (Just x, Just z) -> Right (x, z)
  _ -> Left "Missing chunk x or z pos"

chunks :: Tag -> Maybe Chunk
chunks t = Chunk <$> xPos t <*> zPos t <*> sections t

cube
  :: (Int, Int, Int)
  -> [Int] -- indices into [a]
  -> [a] -- descriptions of each block
  -> [[[a]]] -- folded 3d cube
cube dimension longs names = foldDimensions dimension [names !! i | i <- concatMap (unpackInt (length names)) longs]

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

-- | stack many cubes (e.g., 16X16X16 sectors) into columns on the y-axis
stackBlocksY :: [[[[B.ByteString]]]] -> [[[B.ByteString]]]
stackBlocksY = concat


-- | A 64 bit long is a long thing, no need to waste space. If the number of
-- things being stored is only require 32 bits, we might as well store two
-- things. If they require 21 bytes, we can store 3. And so on.
unpackInt
  :: Int -- the number of things indexed
  -> Int -- 64-bit integer storing packed indices
  -> [Int]  -- unpacked indices
unpackInt size x =
  let nbits = max 4 (ceiling (logBase 2 (fromIntegral size)))
      k = div 64 nbits -- integer division
      modder = Bits.shiftL 1 nbits
  in [Bits.rotateR x ((i-1) * nbits) `mod` modder | i <- [1 .. k]] 

sections :: Tag -> Maybe [[[B.ByteString]]]
sections (TagList _ TagCompoundId "Sections" xs)
  = stackBlocksY . map (uncurry (cube (16,16,16))) <$> mapM section xs
sections (TagCompound _ xs) = case DM.mapMaybe sections xs of
  [x] -> Just x
  _ -> Nothing
sections _ = Just []

section :: Tag -> Maybe ([Int], [B.ByteString])
section (TagCompound "" xs) = seekTwo blockState paletteNames xs

blockState :: Tag -> Maybe [Int]
blockState (TagLongArray "BlockStates" xs) = Just xs
blockState _ = Nothing

paletteNames :: Tag -> Maybe [B.ByteString]
paletteNames (TagList _ TagCompoundId "Palette" xs) = mapM inCompound xs
  where
    inCompound :: Tag -> Maybe B.ByteString
    inCompound (TagCompound "" ys) = seek name ys
    inCompound _ = Nothing

name :: Tag -> Maybe B.ByteString
name (TagString "Name" name) = Just name
name _ = Nothing
