module Mastermind.Test
  ( mastermindTests
  ) where

import Mastermind.Core
import Test.HUnit

testRemoveColor =
  let colors0 = []
      colors1 = map Color ["red"]
      colors2 = map Color ["red", "blue"]
      colors3 = map Color ["red", "red"]
  in TestCase (do assertEqual "For empty list" [] (removeColor colors0 (Color "red"))
                  assertEqual "For 1 elem" [] (removeColor colors1 (Color "red"))
                  assertEqual "For 2 /= elems" [Color "red"] (removeColor colors2 (Color "blue"))
                  assertEqual "For 2 == elems" [Color "red"] (removeColor colors3 (Color "red")))

testGetWellPlaced =
  let secret0 = []
      guess0 = []
      secret1 = map Color ["red", "blue", "blue", "red"]
      guess1 = map Color ["red", "red", "blue", "blue"]
  in TestCase (do assertEqual "For empty lists" 0 (getWellPlaced secret0 guess0)
                  assertEqual "For 2 well placed" 2 (getWellPlaced secret1 guess1))

testGetMissPlaced =
  let secret0 = []
      guess0 = []
      secret1 = map Color ["blue"]
      guess1 = map Color ["red"]
      secret2 = map Color ["blue"]
      guess2 = map Color ["blue"]
      secret3 = map Color ["blue", "red", "blue"]
      guess3 = map Color ["blue", "blue", "blue"]
  in TestCase (do assertEqual "For empty lists" 0 (getMissPlaced secret0 guess0)
                  assertEqual "For 1 non matching" 0 (getMissPlaced secret1 guess1)
                  assertEqual "For 1 matching" 1 (getMissPlaced secret2 guess2)
                  assertEqual "For only 2 matching" 2 (getMissPlaced secret3 guess3))
               
testRemoveWellPlaced =
  let secret0 = []
      guess0 = []
      secret1 = map Color ["blue"]
      guess1 = map Color ["blue"]
      secret2 = map Color ["blue", "red", "green", "yellow"]
      guess2 = map Color ["blue", "green", "red", "yellow"]
  in TestCase (do assertEqual "For empty lists" ([], []) (removeWellPlaced secret0 guess0)
                  assertEqual "For only matching elements" ([], []) (removeWellPlaced secret1 guess1)
                  assertEqual "For usual cases" ((map Color ["red", "green"]), (map Color ["green", "red"])) (removeWellPlaced secret2 guess2))

mastermindTests = TestList [ TestLabel "testRemoveColor" testRemoveColor
                           , TestLabel "testGetWellPlaced" testGetWellPlaced
                           , TestLabel "testGetMissPlaced" testGetMissPlaced
                           , TestLabel "testRemoveWellPlaced" testRemoveWellPlaced]
