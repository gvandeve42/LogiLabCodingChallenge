module Mastermind.Core
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
           scanColor color =
             (color, nbColor)
             where
               nbColor = wellPlaced . submitGuess . replicate secretLength $ color

           --scanColors :: (Ord a) => [Color a] -> [Color a]
           scanColors colors =
             map fst . filter ((>0) . snd) . map scanColor $ colors

           --createGuess :: (Ord a) => Color a -> Int -> ([Color a], Int)
           createGuess color index =
             ((replicate (index - 1) O)++[color]++(replicate (secretLength - index) O), index)

           --getIndexes :: (Ord a) => Color a -> [Int]
           getIndexes color =
             filter (>0) . map isIndex $ guesses
             where
               guesses = map (createGuess color) [1..secretLength]
               isIndex guess = if (wellPlaced . submitGuess .fst $ guess) == 1
                               then (snd guess)
                               else 0

           --getColorPos :: (Ord a) => [Color a] -> [(Color a, [Int])]
           getColorPos colors =
             [(color, (getIndexes color)) | color <- colors]

           --getFromIndex :: (Ord a) => [(Color a, [Int])] -> Int -> Color a
           getFromIndex [] _ = O
           getFromIndex (x:xs) index =
             if elem index (snd x)
             then (fst x)
             else getFromIndex xs index

           --buildResult :: (Ord a) => [(Color a, Int)] -> [Color a]
           buildResult colorPos =
             map (getFromIndex colorPos) [1..secretLength]
       in Just . buildResult . getColorPos $ colors
