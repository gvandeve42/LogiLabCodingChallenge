module Main
  ( main
  ) where

import System.Environment
import System.Random
import Control.Monad
import Mastermind.Models
import Mastermind.Player


getRandomValue :: (Ord a) => [Color a] -> IO (Color a)
getRandomValue colors = do
  index <- randomRIO (0, length colors - 1)
  return $ colors!!index

generateSecret :: (Ord a) => Int -> [Color a] -> IO [Color a]
generateSecret n colors = do
  secret <- replicateM n $ getRandomValue colors
  return secret

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then do putStrLn "Error in the number of arguments... Please retry"
            return ()
    else do let n = (read $ args!!0) :: Int
            let colors = map Color [1..(read $ args!!1)] :: [Color Int]
            secret <- generateSecret n colors
            putStrLn $ "Secret: length = "++(show . length $ secret)
            putStrLn $ "Colors: length = "++(show . length $ colors)
            let result = calcResult secret colors
            putStrLn . show $ result
            return ()
             
