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



