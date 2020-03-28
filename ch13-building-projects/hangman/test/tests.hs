-- tests/tests.hs

module Main where

import Hangman
import Test.QuickCheck
import Data.Maybe (isJust)
import Control.Applicative

getWord :: Puzzle -> String
getWord (Puzzle w _ _) = w
getDiscoveredChar :: Puzzle -> [Maybe Char]
getDiscoveredChar (Puzzle _ d _ ) = filter (\x -> isJust x) d

getGuessed :: Puzzle -> [Char]
getGuessed (Puzzle _ _ g) = g

charGen :: Gen Char
charGen = elements (['a'..'z'] ++ ['A'..'Z'])

wordGen :: Gen String
wordGen = listOf1 charGen 


wordsCharGen :: String -> Gen Char
wordsCharGen word = elements word

notInWordCharGen :: String -> Gen Char
notInWordCharGen word =  suchThat (arbitrary::Gen Char) (\x -> not(elem x word)) 

prop_discovered:: Property
prop_discovered = forAll (do
  word <- wordGen
  ch <- wordsCharGen word
  return (word, ch))
  (\(word, ch) ->  getDiscoveredChar (fillInCharacter (freshPuzzle word) ch)  == fmap (const Just ch) (filter (==ch) word))

prop_guessed::Property
prop_guessed = forAll (do
  word <- wordGen
  ch <- notInWordCharGen word 
  return (word, ch))
  (\(word, ch) -> getGuessed(fillInCharacter (freshPuzzle word) ch) == [ch])

prop_handlePuzzle_alreadyGuessed :: Property
prop_handlePuzzle_alreadyGuessed = forAll (do
  word <- wordGen
  ch <- wordsCharGen word
  let puzzle = fillInCharacter (freshPuzzle word) ch
  return (word, ch, puzzle))
  (\(word, ch, puzzle) -> ioProperty (fmap (==puzzle)(handleGuess puzzle ch)))


prop_handlePuzzle_inTheWord :: Property
prop_handlePuzzle_inTheWord = forAll (do
  word <- wordGen
  ch <- wordsCharGen word
  let puzzle = freshPuzzle word 
  return (word, ch, puzzle))
  (\(word, ch, puzzle) -> ioProperty (fmap (==fillInCharacter puzzle ch)(handleGuess puzzle ch)))


prop_handlePuzzle_notInTheWord :: Property
prop_handlePuzzle_notInTheWord = forAll (do
  word <- wordGen
  ch <-  notInWordCharGen word
  let puzzle = freshPuzzle word 
  return (word, ch, puzzle))
  (\(word, ch, puzzle) -> ioProperty (fmap (==fillInCharacter puzzle ch)(handleGuess puzzle ch)))



main :: IO ()
main = do
    quickCheck prop_discovered
    quickCheck prop_guessed
    quickCheck prop_handlePuzzle_alreadyGuessed
    quickCheck prop_handlePuzzle_inTheWord
    quickCheck prop_handlePuzzle_notInTheWord
