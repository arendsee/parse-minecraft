module ParseMinecraft.Search
  ( xPos
  , yPos
  ) where

import qualified Data.Maybe as DM

import ParseMinecraft.Namespace

findShortByName :: Name -> Tag -> Maybe Int
findShortByName name (TagShort tagName x)
  | tagName == name = Just x
  | otherwise = Nothing
findShortByName name (TagCompound _ xs) = (DM.listToMaybe . DM.mapMaybe (findShortByName name)) xs
findShortByName name (TagList _ TagShortId _ xs) = (DM.listToMaybe . DM.mapMaybe (findShortByName name)) xs

xPos :: Tag -> Maybe Int
xPos = findShortByName "xPos"

yPos :: Tag -> Maybe Int
yPos = findShortByName "yPos"

