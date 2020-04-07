module Main where

import SemigroupExercises
import MonoidExercises

main :: IO ()
main = do
  SemigroupExercises.runQuickCheck 
  MonoidExercises.runQuckCheck
