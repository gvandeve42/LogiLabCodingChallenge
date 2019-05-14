module Mastermind.Utils
  ( countColor
  ) where

import Mastermind.Models

countColor :: (Ord a) => [Color a] -> Color a -> Int
-- ^Simple function for counting the number of occurence of a color in a list of colors
countColor [] _ = 0
countColor (x:xs) color
  | x == color = succ (countColor xs color)
  | otherwise = countColor xs color
