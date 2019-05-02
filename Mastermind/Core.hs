module Mastermind.Core
  ( Result(..)
  , Color(..)
  , Guess(..)
  , Secret(..)
  , secretExample
  , guessExample
  , removeColor
  , getWellPlaced
  , getMissPlaced
  , removeWellPlaced
  , getResult
  ) where

import Data.List

data Result = Result {wellPlaced :: Int, missPlaced :: Int} deriving (Show)
newtype Color = Color String deriving (Eq, Show)
type Guess = [Color]
type Secret = [Color]

secretExample :: Secret
secretExample = map Color ["blue", "red", "green", "pink", "yellow", "blue", "blue", "yellow"]

guessExample :: Guess
guessExample = map Color ["yellow", "red", "blue", "green", "green", "yellow", "blue", "blue"]

removeColor :: [Color] -> Color -> [Color]
-- ^Remove one and only one color from a list of colors.
removeColor [] _ = []
removeColor (x:xs) color
  | x == color = xs
  | otherwise = x:(removeColor xs color)

getWellPlaced :: Secret -> Guess -> Int
-- ^Accumulator for counting the well placed colors.
getWellPlaced [] [] = 0
getWellPlaced (x:xs) (y:ys)
  | x == y = succ $ getWellPlaced xs ys
  | otherwise = getWellPlaced xs ys

getMissPlaced :: Secret -> Guess -> Int
-- ^ Count the number of miss placed colors.
-- /!\ Should only be used after applying the "removeWellPlaced" function
getMissPlaced _ [] = 0
getMissPlaced secret (x:xs)
  | elem x secret = succ (getMissPlaced newSecret xs)
  | otherwise = getMissPlaced secret xs
  where
    newSecret = removeColor secret x

removeWellPlaced :: Secret -> Guess -> (Secret, Guess)
-- ^Remove the identical pairs for further analysis
removeWellPlaced secret guess =
  let tuplify [] = ([], [])
      tuplify [a, b] = (a, b)
      pairMissPlaced [] [] = []
      pairMissPlaced (x:xs) (y:ys)
        | x == y = pairMissPlaced xs ys
        | otherwise = [x, y]:(pairMissPlaced xs ys)
  in tuplify . transpose . pairMissPlaced secret $ guess

getResult :: Secret -> Guess -> Result
getResult secret guess =
  Result wellPlaced missPlaced
  where
    (newSecret, newGuess) = removeWellPlaced secret guess
    wellPlaced = getWellPlaced secret guess
    missPlaced = getMissPlaced newSecret newGuess
