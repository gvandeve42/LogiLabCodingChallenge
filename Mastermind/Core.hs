module Mastermind.Core
  ( secretExample
  , guessExample
  ) where

import Data.List
import Mastermind.Models
import Mastermind.HintPlayer

secretExample :: [Color String]
secretExample = map Color ["blue", "red", "green", "pink", "yellow", "blue", "blue", "yellow"]

guessExample :: [Color String]
guessExample = map Color ["yellow", "red", "blue", "green", "green", "yellow", "blue", "blue"]

colorsExample :: [Color String]
colorsExample = sort . map Color $ ["blue", "red", "green", "pink", "yellow", "orange", "purple", "yoyo", "yaya"]

secretLength :: Int
secretLength = 8

--submitGuess :: (Ord a) => [Color a] -> Result
submitGuess guess = getResult secretExample guess

--------------------- Get wich colors and their numbers ---------------------

--completeGuess :: (Ord a) => Color a -> Int -> [Color a] -> [Color a]
completeGuess color secretLength incompleteGuess =
  incompleteGuess++colorLst
  where
    colorLst = replicate n color
    n = secretLength - (length incompleteGuess)

--scanColor :: (Ord a) => Int -> Color a -> (Color a, Int)
scanColor secretLength color =
  (color, nbColor)
  where
    nbColor = wellPlaced . submitGuess . replicate secretLength $ color

--scanColors :: (Ord a) => Int -> [Color a] -> [Color a]
scanColors secretLength colors =
  map fst . filter ((>0) . snd) . map (scanColor secretLength) $ colors

------------------------------------------------------------

--------------------- Resolve the Mastermind ---------------------

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

getFromIndex :: (Ord a) => [(Color a, [Int])] -> Int -> Color a
getFromIndex [] _ = O
getFromIndex (x:xs) index =
  if elem index (snd x)
  then (fst x)
  else getFromIndex xs index

buildResult :: (Ord a) => Int -> [(Color a, Int)] -> [Color a]
buildResult secretLength colorPos =
  map getFromIndex colorPos

calcResult :: (Ord a) => [Color a] -> [Color a] -> Maybe [Color a]
calcResult [] _ = Nothing
calcResult _ [] = Nothing
calcResult secret colors
  | not . and . map ((flip elem) colors) secret = Nothing
  | otherwise = Just . buildResult . secretLength $ colorPos
  where
    secretLength = length secret
    submitGuess = getResult . secret
    colorPos = getColorPos colors
                    
