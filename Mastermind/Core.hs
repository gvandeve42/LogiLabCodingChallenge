module Mastermind.Core
  ( Result(..)
  , Color(..)
  , secretExample
  , guessExample
  , countColor
  , getWellPlaced
  , getColorsCount
  , getResult
  ) where

import Data.List

data Result = Result {wellPlaced :: Int, missPlaced :: Int} deriving (Eq, Show)
newtype Color a = Color a deriving (Eq, Ord, Show)

secretExample :: [Color String]
secretExample = map Color ["blue", "red", "green", "pink", "yellow", "blue", "blue", "yellow"]

guessExample :: [Color String]
guessExample = map Color ["yellow", "red", "blue", "green", "green", "yellow", "blue", "blue"]

countColor :: (Ord a) => [Color a] -> Color a -> Int
-- ^Simple function for counting the number of occurence of a color in a list of colors
countColor [] _ = 0
countColor (x:xs) color
  | x == color = succ (countColor xs color)
  | otherwise = countColor xs color

getWellPlaced :: (Ord a) => [Color a] -> [Color a] -> Int
-- ^Accumulator for counting the well placed colors.
getWellPlaced [] [] = 0
getWellPlaced (x:xs) (y:ys)
  | x == y = succ $ getWellPlaced xs ys
  | otherwise = getWellPlaced xs ys

getColorsCount :: (Ord a) => [Color a] -> [Color a] -> [Color a] -> Int
getColorsCount [] [] _ = 0
getColorsCount secret guess colors =
  sum . map (minColorCount) $ colors
  where
    minColorCount color = min (countColor secret color) (countColor guess color)

--getResult :: (Ord a) => [Color a] -> [Color a] -> [Color a] -> Maybe Result
getResult :: (Ord a) => [Color a] -> [Color a] -> Maybe Result
--getResult secret guess colors =
getResult secret guess =
  if length secret == length guess
  then Just . Result wellPlaced $ missPlaced
  else Nothing
  where
    colors = nub secret
    wellPlaced = getWellPlaced secret guess
    missPlaced = (getColorsCount secret guess colors) - wellPlaced
