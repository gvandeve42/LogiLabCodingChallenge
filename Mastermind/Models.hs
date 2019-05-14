module Mastermind.Models
  ( Color(..)
  , Result(..)
  ) where

newtype Color a = Color a deriving (Eq, Ord, Show)

data Result = Result {wellPlaced :: Int, missPlaced :: Int} deriving (Eq, Show)
