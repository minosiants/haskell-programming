
module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), 
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do 
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = 
            let  l = length (w :: String)
            in   l >= minWordLength
              &&  l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO(0, (length wl) -1)
    return $ wl !! randomIndex

randomWord' ::  IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ 
     fmap renderPuzzleChar discovered)
     ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) []

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just ch) = ch
renderPuzzleChar Nothing = '_'
  

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) ch = elem ch str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = 
    elem ch guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c:s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
          , alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You already guessed that character, pick something else"
        return puzzle
      (True, _) -> do
        putStrLn "This character was in the word, filling in the word accordingly"
        return (fillInCharacter puzzle guess)
      (False, _) -> do 
        putStrLn "This character was not in the word, try again."
        return (fillInCharacter puzzle guess)


countSuccess :: [Maybe Char] -> Int
countSuccess l = length $ filter isJust l 
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guessed) = 
   if (length guessed - countSuccess filledIn ) > (length wordToGuess) then
      do putStrLn "You lose !"
         putStrLn $
            "The word was: " ++ wordToGuess
         exitSuccess
   else  return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
   if all isJust filledInSoFar then
      do putStrLn "You win!"
         exitSuccess
   else return ()
   
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Currient puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess should be a single character"
    
