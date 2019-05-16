module Mastermind.Player
  ( calcResult
  ) where

import Data.List
import Mastermind.Models
import Mastermind.HintPlayer

calcResult :: (Ord a) => [Color a] -> [Color a] -> Maybe [Color a]
calcResult [] _ = Nothing
calcResult _ [] = Nothing
calcResult secret colors =
  if not . and . map ((flip elem) colors) $ secret then Nothing
  else let secretLength = length secret
           submitGuess = getResult secret

           --scanColor :: (Ord a) => Color a -> (Color a, Int)
           -- ^For a color get the number of times it is present in the secret
           scanColor color =
             (color, nbColor)
             where
               nbColor = wellPlaced . submitGuess . replicate secretLength $ color

           --scanColors :: (Ord a) => [Color a] -> [Color a]
           -- ^Get the result of scanColor for all colors
           scanColors colors =
             map fst . filter ((>0) . snd) . map scanColor $ colors

           --createGuess :: (Ord a) => Color a -> Int -> ([Color a], Int)
           -- ^Create a guess for determining the presence of a color at a given position
           createGuess color index =
             ((replicate (index - 1) O)++[color]++(replicate (secretLength - index) O), index)

           --getIndexes :: (Ord a) => Color a -> [Int]
           -- ^For a color get all the indexes it is present in the secret
           getIndexes color =
             filter (>0) . map isIndex $ guesses
             where
               guesses = map (createGuess color) [1..secretLength]
               isIndex guess = if (wellPlaced . submitGuess .fst $ guess) == 1
                               then (snd guess)
                               else 0

           --getColorPos :: (Ord a) => [Color a] -> [(Color a, [Int])]
           -- ^For all the present colors in the secret, get the positions of each of them.
           getColorPos colors =
             [(color, (getIndexes color)) | color <- colors]

           --getFromIndex :: (Ord a) => [(Color a, [Int])] -> Int -> Color a
           -- ^Transform an index into a color with the help of the others functions
           getFromIndex [] _ = O
           getFromIndex (x:xs) index =
             if elem index (snd x)
             then (fst x)
             else getFromIndex xs index

           --buildResult :: (Ord a) => [(Color a, Int)] -> [Color a]
           -- ^Build the new response string
           buildResult colorPos =
             map (getFromIndex colorPos) [1..secretLength]
       in Just . buildResult . getColorPos $ colors
