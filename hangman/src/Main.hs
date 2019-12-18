module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <-readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int 
maxWordLength = 9 

randomWord :: WordList -> IO String
randomWord wl = do 
  randomIndex <- randomRIO (0 ,length wl - 1  )
  return $ wl !! randomIndex

gameWords :: IO WordList
gameWords = do 
  aw <- allWords
  return (filter gameLength aw)
    where gameLength w = (length (w :: String) >= minWordLength) && (length (w :: String) < maxWordLength)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle a = Puzzle a (map (\x -> Just x) a) []

charInWorld :: Puzzle -> Char -> Bool
charInWorld (Puzzle a _ _) c = c `elem` a 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g ) c = c `elem` g 

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c 

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar = if wordChar == guessed
                                              then Just wordChar
                                              else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWorld puzzle guess, alreadyGuessed puzzle guess) of 
    (_, True) -> do
     putStrLn "You already guessed that\ \ character, pick \ \ something else!"
     return puzzle
    (True, _) -> do
     putStrLn "This character was in the\ \ word, filling in the word\ \accordinly"
     return (fillInCharacter puzzle guess)
    (False, _) -> do
     putStrLn "This character wasn't in\ \ the owrd, try again."
     return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $"The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle ->IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
  gameOver puzzle
  gameWin puzzle 
  putStrLn $ "Current puzzle is: "++ show puzzle 
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame 
    _ -> putStr "Your guess must\ \ be a single character"
  
main :: IO ()
main = do 
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word) 
  runGame puzzle


