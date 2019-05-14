module Mastermind.Models
  ( Color(..)
  , Result(..)
  ) where

data Color a = O | Color a deriving (Eq, Ord, Show)

data Result = Result {wellPlaced :: Int, missPlaced :: Int} deriving (Eq, Show)

