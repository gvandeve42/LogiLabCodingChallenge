module Mastermind.Test
  ( mastermindTests
  ) where

import Mastermind.Models
import Mastermind.HintPlayer
import Mastermind.Utils
import Test.HUnit

testGetWellPlaced =
  let secret0 = [] :: [Color String]
      guess0 = [] :: [Color String]
      secret1 = map Color ["red", "blue", "blue", "red"]
      guess1 = map Color ["red", "red", "blue", "blue"]
  in TestCase (do assertEqual "For empty lists" 0 (getWellPlaced secret0 guess0)
                  assertEqual "For 2 well placed" 2 (getWellPlaced secret1 guess1))

testCountColor =
  let list0 = []
      color0 = Color "red"
      list1 = map Color ["red", "red", "red", "blue", "blue"]
      color1 = Color "blue"
      color2 = Color "red"
      list3 = map Color ["red", "red", "red", "blue", "blue"]
      color3 = Color "green"
  in TestCase (do assertEqual "For empty lists" 0 (countColor list0 color0)
                  assertEqual "For 2 matchs" 2 (countColor list1 color1)
                  assertEqual "For 3 matchs same lst diff color" 3 (countColor list1 color2)
                  assertEqual "For 0 matchs non existing color" 0 (countColor list3 color3))

testGetColorsCount =
  let colors = map Color ["red", "blue"]
      secret0 = []
      guess0 = []
      secret1 = map Color ["blue"]
      guess1 = map Color ["red"]
      secret2 = map Color ["blue"]
      guess2 = map Color ["blue"]
      secret3 = map Color ["blue", "red", "blue"]
      guess3 = map Color ["blue", "blue", "blue"]
      secret4 = map Color ["blue", "red", "blue", "red"]
      guess4 = map Color ["blue", "blue", "red", "red"]
  in TestCase (do assertEqual "For empty lists" 0 (getColorsCount secret0 guess0 colors)
                  assertEqual "For 1 non matching" 0 (getColorsCount secret1 guess1 colors)
                  assertEqual "For 1 matching" 1 (getColorsCount secret2 guess2 colors)
                  assertEqual "For 2 matching" 2 (getColorsCount secret3 guess3 colors)
                  assertEqual "For 4 matching" 4 (getColorsCount secret4 guess4 colors))
               
testGetResult =
  let secret0 = [] :: [Color String]
      guess0 = [] :: [Color String]
      secret1 = map Color ["blue", "red"]
      guess1 = map Color ["blue", "red"]
      secret2 = map Color ["blue", "red"]
      guess2 = map Color ["red", "blue"]
      secret3 = map Color ["blue", "red", "green", "yellow", "green", "green", "green"]
      guess3 = map Color ["blue", "green", "red", "yellow", "red", "red", "red"]
      secret4 = map Color ["blue", "red", "green", "yellow", "green", "green", "green", "orange"]
      guess4 = map Color ["blue", "green", "red", "yellow", "red", "red", "red"]
  in TestCase (do assertEqual "For empty lists" (Just . Result 0 $ 0) (getResult secret0 guess0)
                  assertEqual "For only 2 well placed colors" (Just . Result 2 $ 0) (getResult secret1 guess1)
                  assertEqual "For only 2 miss placed colors" (Just . Result 0 $ 2) (getResult secret2 guess2)
                  assertEqual "For 2 well and 2 miss placed colors" (Just . Result 2 $ 2) (getResult secret3 guess3)
                  assertEqual "For lists with non equal length" Nothing (getResult secret4 guess4))

mastermindTests = TestList [ TestLabel "testCountColor" testCountColor
                           , TestLabel "testGetWellPlaced" testGetWellPlaced
                           , TestLabel "testGetColorsCount" testGetColorsCount
                           , TestLabel "testGetResult" testGetResult]
