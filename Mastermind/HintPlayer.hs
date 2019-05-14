module Mastermind.HintPlayer
  ( getWellPlaced
  , getColorsCount
  , getResult
  ) where

import Data.List (nub)
import Mastermind.Models
import Mastermind.Utils

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
getResult :: (Ord a) => [Color a] -> [Color a] -> Result
--getResult secret guess colors =
getResult secret guess =
  Result wellPlaced $ missPlaced
  where
    colors = nub secret
    wellPlaced = getWellPlaced secret guess
    missPlaced = (getColorsCount secret guess colors) - wellPlaced
